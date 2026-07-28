package ph.samson.atbp.liga.tournament

import better.files.File
import ph.samson.atbp.liga.io.PeriodCodec
import ph.samson.atbp.liga.io.PeriodWriter
import ph.samson.atbp.liga.model.*
import zio.Task
import zio.ZIO

import java.time.LocalDate

/** Build and write a completed-tournament period file to the data root. */
object PeriodEmission {

  final case class EmissionError(message: String) extends Exception(message)

  def periodFilename(name: String, completed: LocalDate): String =
    s"$completed-${Resume.slugify(name)}.liga"

  def boardToRack(
      scoreA: Int,
      scoreB: Int,
      weaker: Player,
      playerA: Player,
      playerB: Player,
      handicapApplied: Int
  ): (Int, Int) =
    if (weaker == playerA) {
      (math.max(0, scoreA - handicapApplied), scoreB)
    } else if (weaker == playerB) {
      (scoreA, math.max(0, scoreB - handicapApplied))
    } else {
      (scoreA, scoreB)
    }

  def toPeriod(
      state: TournamentState,
      completed: LocalDate
  ): Either[String, Period] =
    toPeriodWithMatchIds(state, completed).map(_._1)

  /** Period plus bracket match IDs in the same order as `period.matches`. */
  def toPeriodWithMatchIds(
      state: TournamentState,
      completed: LocalDate
  ): Either[String, (Period, List[String])] =
    for {
      _ <- validateReady(state)
      bracket <- state.bracket.toRight("no bracket loaded")
      rows <- bracket.matches
        .filter(matchHasResult)
        .sortBy(_.id)
        .foldLeft(Right(Nil): Either[String, List[(PeriodMatch, String)]]) {
          case (Right(acc), matchDef) =>
            toPeriodMatch(state, matchDef)
              .map(row => acc :+ (row -> matchDef.id))
          case (left, _) => left
        }
    } yield {
      val (matches, matchIds) = rows.unzip
      Period(
        name = state.name,
        completed = completed,
        matches = matches
      ) -> matchIds
    }

  def write(
      dataDir: File,
      state: TournamentState,
      completed: LocalDate
  ): Task[File] =
    for {
      built <- ZIO.fromEither(
        toPeriodWithMatchIds(state, completed).left.map(EmissionError(_))
      )
      (period, matchIds) = built
      target = dataDir / periodFilename(state.name, completed)
      exists <- ZIO.attemptBlocking(target.exists)
      _ <- ZIO.when(exists) {
        ZIO.fail(
          EmissionError(
            s"period file already exists: ${target.pathAsString}"
          )
        )
      }
      _ <- ZIO.attemptBlocking(
        dataDir.createDirectoryIfNotExists(createParents = true)
      )
      hocon <- ZIO.fromEither(
        PeriodWriter.write(period, matchIds).left.map(EmissionError(_))
      )
      _ <- ZIO.attemptBlocking(target.write(hocon))
    } yield target

  /** Write a new period file, or verify an existing file matches expected
    * content.
    *
    * Used by tournament completion retry: if the period was written but the
    * completion event append failed, a retry must not overwrite the file.
    */
  def writeOrVerify(
      dataDir: File,
      state: TournamentState,
      completed: LocalDate
  ): Task[Unit] =
    for {
      built <- ZIO.fromEither(
        toPeriodWithMatchIds(state, completed).left.map(EmissionError(_))
      )
      (period, matchIds) = built
      target = dataDir / periodFilename(state.name, completed)
      exists <- ZIO.attemptBlocking(target.exists)
      _ <-
        if (!exists) {
          for {
            _ <- ZIO.attemptBlocking(
              dataDir.createDirectoryIfNotExists(createParents = true)
            )
            hocon <- ZIO.fromEither(
              PeriodWriter.write(period, matchIds).left.map(EmissionError(_))
            )
            _ <- ZIO.attemptBlocking(target.write(hocon))
          } yield ()
        } else {
          for {
            onDisk <- PeriodCodec.parseFile(target)
            _ <-
              if (onDisk == period) {
                ZIO.unit
              } else {
                ZIO.fail(
                  EmissionError(
                    s"period file content mismatch: ${target.pathAsString}"
                  )
                )
              }
          } yield ()
        }
    } yield ()

  private def validateReady(state: TournamentState): Either[String, Unit] =
    for {
      bracket <- state.bracket.toRight("no bracket loaded")
      incomplete = bracket.matches.filter(
        _.state != BracketMatchState.Completed
      )
      _ <-
        if (incomplete.isEmpty) {
          Right(())
        } else {
          Left(
            "tournament has incomplete matches: " +
              incomplete.map(_.id).sorted.mkString(", ")
          )
        }
      resultMatches = bracket.matches.filter(matchHasResult)
      _ <-
        if (resultMatches.nonEmpty) {
          Right(())
        } else {
          Left("tournament has no recorded match results")
        }
    } yield ()

  private def matchHasResult(matchDef: BracketMatch): Boolean =
    matchDef.result.isDefined &&
      matchDef.playerA.nonEmpty &&
      matchDef.playerB.nonEmpty

  private def toPeriodMatch(
      state: TournamentState,
      matchDef: BracketMatch
  ): Either[String, PeriodMatch] =
    for {
      playerA <- matchDef.playerA.toRight(s"missing player A in ${matchDef.id}")
      playerB <- matchDef.playerB.toRight(s"missing player B in ${matchDef.id}")
      result <- matchDef.result.toRight(s"missing result in ${matchDef.id}")
      ratingA <- frozenRating(state, playerA)
      ratingB <- frozenRating(state, playerB)
      raceTo <- MatchLifecycle
        .resolveRaceTo(state, matchDef.id)
        .left
        .map(_.message)
      weaker = weakerPlayer(ratingA, ratingB)
      handicapSuggested = matchDef.handicapSuggested.getOrElse(0)
      handicapApplied = matchDef.handicapApplied.getOrElse(0)
      (rackA, rackB) = boardToRack(
        result.scoreA,
        result.scoreB,
        weaker,
        playerA,
        playerB,
        handicapApplied
      )
    } yield PeriodMatch(
      playerA = playerA,
      playerB = playerB,
      scoreA = rackA,
      scoreB = rackB,
      raceTo = raceTo,
      handicapSuggested = handicapSuggested,
      handicapApplied = handicapApplied
    )

  private def frozenRating(
      state: TournamentState,
      player: Player
  ): Either[String, PlayerRating] =
    state.frozenRatings
      .get(player)
      .toRight(s"no frozen rating for ${player.name}")

  private def weakerPlayer(a: PlayerRating, b: PlayerRating): Player =
    if (a.rating != b.rating) {
      if (a.rating < b.rating) a.player else b.player
    } else {
      List(a, b).minBy(_.player.name).player
    }
}
