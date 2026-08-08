package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.bracket.RaceToScopes
import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.api.Models.BracketMatchState
import ph.samson.atbp.liga.model.MatchSide

import java.time.Duration
import java.time.Instant

/** Pure bracket layout helpers for the director UI. */
object BracketLayout {

  enum Section(val label: String) {
    case Winners extends Section("Winners")
    case Losers extends Section("Losers")
    case GrandFinal extends Section("Grand Final")
    case SingleElimination extends Section("Single Elimination")
  }

  /** Layout round for `gf-2` (reset grand final); distinct from `gf-1`
    * grouping.
    */
  val GrandFinalResetRound: Int = 0

  final case class RoundGroup(
      section: Section,
      round: Int,
      matches: List[BracketMatch]
  )

  final case class DirectorBracketSections(
      liveStrip: List[BracketMatch],
      readyStrip: List[BracketMatch],
      groups: List[RoundGroup]
  )

  def sectionOf(matchId: String): Section =
    matchId match {
      case s if s.startsWith("wb-") => Section.Winners
      case s if s.startsWith("lb-") => Section.Losers
      case s if s.startsWith("se-") => Section.SingleElimination
      case _                        => Section.GrandFinal
    }

  def groupLabel(section: Section, round: Int): String =
    section match {
      case Section.GrandFinal if round == GrandFinalResetRound =>
        s"${section.label} — reset"
      case Section.GrandFinal => section.label
      case _                  => s"${section.label} — round $round"
    }

  def matchLabel(matchId: String, bracketSize: Int): String =
    groupLabel(sectionOf(matchId), roundOf(matchId, bracketSize))

  private def log2(n: Int): Int =
    (math.log(n) / math.log(2)).toInt

  /** Bracket round encoded in match ids (`wb-2-1`, `lb-3-2`, `gf-1`). */
  def bracketRound(matchId: String, bracketSize: Int): Option[Int] =
    matchId match {
      case s"wb-$round-$_" => round.toIntOption
      case s"lb-$round-$_" => round.toIntOption
      case s"se-$round-$_" => round.toIntOption
      case "gf-1"          => Some(log2(bracketSize))
      case "gf-2"          => Some(GrandFinalResetRound)
      case _               => None
    }

  def roundOf(matchId: String, bracketSize: Int): Int =
    bracketRound(matchId, bracketSize).getOrElse(0)

  private def showInList(m: BracketMatch): Boolean =
    if (
      m.state == BracketMatchState.Completed && m.isBye &&
      m.playerA.isEmpty && m.playerB.isEmpty
    ) {
      false
    } else {
      m.state != BracketMatchState.Pending ||
      m.playerA.isDefined || m.playerB.isDefined
    }

  private def statusSortKey(m: BracketMatch): Int =
    m.state match {
      case BracketMatchState.Ready     => 0
      case BracketMatchState.Pending   => 1
      case BracketMatchState.Started   => 2
      case BracketMatchState.Completed => 3
    }

  private def matchSeedIndex(matchId: String): Int =
    matchId match {
      case s"wb-$_-$index" => index.toIntOption.getOrElse(0)
      case s"lb-$_-$index" => index.toIntOption.getOrElse(0)
      case s"se-$_-$index" => index.toIntOption.getOrElse(0)
      case "gf-1"          => 1
      case "gf-2"          => 2
      case _               => 0
    }

  private def matchSortKey(m: BracketMatch): (Int, Int) =
    (statusSortKey(m), matchSeedIndex(m.id))

  private def listSectionOrder(section: Section): Int =
    section match {
      case Section.GrandFinal        => 0
      case Section.SingleElimination => 1
      case Section.Losers            => 2
      case Section.Winners           => 3
    }

  private def roundFullyCompleted(matches: List[BracketMatch]): Boolean =
    matches.forall(_.state == BracketMatchState.Completed)

  private def groupStackKey(
      section: Section,
      round: Int,
      allMatches: List[BracketMatch]
  ): (Int, Int, Int) =
    (
      if (roundFullyCompleted(allMatches)) 1 else 0,
      listSectionOrder(section),
      -round
    )

  private final case class PreparedGroup(
      section: Section,
      round: Int,
      allMatches: List[BracketMatch],
      shownMatches: List[BracketMatch]
  )

  def groupMatches(
      matches: List[BracketMatch],
      bracketSize: Int
  ): List[RoundGroup] =
    matches
      .groupBy(m => (sectionOf(m.id), roundOf(m.id, bracketSize)))
      .toList
      .map { case ((section, round), grouped) =>
        PreparedGroup(
          section = section,
          round = round,
          allMatches = grouped,
          shownMatches = grouped.filter(showInList).sortBy(matchSortKey)
        )
      }
      .filter(_.shownMatches.nonEmpty)
      .sortBy(prepared =>
        groupStackKey(prepared.section, prepared.round, prepared.allMatches)
      )
      .map(prepared =>
        RoundGroup(prepared.section, prepared.round, prepared.shownMatches)
      )

  /** Director-only list: Live strip, Ready strip (longest wait first), then
    * groupMatches.
    */
  def directorGroupMatches(
      matches: List[BracketMatch],
      bracketSize: Int
  ): DirectorBracketSections = {
    val live =
      matches
        .filter(_.state == BracketMatchState.Started)
        .sortBy(m => (roundOf(m.id, bracketSize), matchSeedIndex(m.id)))
    val ready =
      matches
        .filter(_.state == BracketMatchState.Ready)
        .sortBy(m =>
          (
            waitEpochMs(m),
            roundOf(m.id, bracketSize),
            matchSeedIndex(m.id)
          )
        )
    val rest =
      matches.filterNot(m =>
        m.state == BracketMatchState.Started ||
          m.state == BracketMatchState.Ready
      )
    DirectorBracketSections(
      liveStrip = live,
      readyStrip = ready,
      groups = groupMatches(rest, bracketSize)
    )
  }

  private def waitEpochMs(matchDef: BracketMatch): Long =
    matchDef.waitStartedAt
      .flatMap(parseInstantSafe)
      .map(_.toEpochMilli)
      .getOrElse(Long.MaxValue)

  def isPendingWithOne(matchDef: BracketMatch): Boolean =
    matchDef.state == BracketMatchState.Pending &&
      matchDef.playerA.isDefined != matchDef.playerB.isDefined

  def elapsedInstantSource(matchDef: BracketMatch): Option[String] =
    if (matchDef.isBye && matchDef.state == BracketMatchState.Completed) {
      None
    } else if (
      matchDef.state == BracketMatchState.Ready || isPendingWithOne(matchDef)
    ) {
      matchDef.waitStartedAt
    } else {
      None
    }

  private def parseInstantSafe(isoInstant: String): Option[Instant] =
    try Some(Instant.parse(isoInstant))
    catch {
      case _: Throwable => None
    }

  private def formatDoneTimeSafe(isoInstant: String): Option[String] =
    parseInstantSafe(isoInstant).map(_ =>
      DirectorTime.formatDoneTime(isoInstant)
    )

  def doneChipText(matchDef: BracketMatch): Option[String] =
    if (matchDef.isBye || matchDef.state != BracketMatchState.Completed) {
      None
    } else {
      matchDef.completedAt.flatMap(formatDoneTimeSafe)
    }

  def startedChipText(matchDef: BracketMatch): Option[String] =
    if (matchDef.state != BracketMatchState.Started) {
      None
    } else {
      matchDef.startedAt.flatMap(formatDoneTimeSafe)
    }

  def newPlayerRestChipText(
      matchDef: BracketMatch,
      now: Instant
  ): Option[String] =
    if (matchDef.state != BracketMatchState.Ready) {
      None
    } else {
      matchDef.newPlayerRestSince.flatMap(formatElapsedSince(_, now))
    }

  /** Director timing chips: Live started clock, Done clock, Ready wait + rest,
    * cooling elapsed.
    */
  def timingChipTexts(matchDef: BracketMatch, now: Instant): List[String] =
    matchDef.state match {
      case BracketMatchState.Completed =>
        doneChipText(matchDef).toList
      case BracketMatchState.Started =>
        startedChipText(matchDef).toList
      case BracketMatchState.Ready =>
        elapsedChipText(matchDef, now).toList ++
          newPlayerRestChipText(matchDef, now).toList
      case BracketMatchState.Pending if isPendingWithOne(matchDef) =>
        elapsedChipText(matchDef, now).toList
      case _ => Nil
    }

  def formatElapsedSeconds(totalSeconds: Long): String = {
    val minutes = totalSeconds / 60
    val hours = minutes / 60
    val mins = minutes % 60
    f"${hours}%02d:${mins}%02d"
  }

  def formatElapsedSince(isoInstant: String, now: Instant): Option[String] =
    parseInstantSafe(isoInstant).map { start =>
      formatElapsedSeconds(
        math.max(0L, Duration.between(start, now).getSeconds)
      )
    }

  def elapsedChipText(matchDef: BracketMatch, now: Instant): Option[String] =
    elapsedInstantSource(matchDef).flatMap(formatElapsedSince(_, now))

  def isActionable(matchDef: BracketMatch): Boolean =
    matchDef.state == BracketMatchState.Ready ||
      matchDef.state == BracketMatchState.Started

  def allMatchesCompleted(matches: List[BracketMatch]): Boolean =
    matches.nonEmpty && matches.forall(_.state == BracketMatchState.Completed)

  def playerLabel(
      player: Option[ph.samson.atbp.liga.js.api.Models.Player]
  ): String =
    player.map(_.name).getOrElse("—")

  def stateLabel(state: BracketMatchState): String =
    state match {
      case BracketMatchState.Pending   => "pending"
      case BracketMatchState.Ready     => "ready"
      case BracketMatchState.Started   => "live"
      case BracketMatchState.Completed => "done"
    }

  def scopeRaceTo(
      matchId: String,
      raceToByScope: Map[String, Int]
  ): Option[Int] =
    RaceToScopes.keyForMatch(matchId).flatMap(raceToByScope.get)

  def scopeKey(section: Section, round: Int): String =
    section match {
      case Section.Winners           => RaceToScopes.keyForWinnersRound(round)
      case Section.Losers            => RaceToScopes.keyForLosersRound(round)
      case Section.SingleElimination =>
        RaceToScopes.keyForSingleElimRound(round)
      case Section.GrandFinal => RaceToScopes.grandFinalScopeKey
    }

  def raceToLabel(raceTo: Int): String =
    s"Race to $raceTo"

  def resolveRoundRaceTo(
      section: Section,
      round: Int,
      raceToByScope: Map[String, Int]
  ): Option[Int] =
    raceToByScope.get(scopeKey(section, round))

  def roundRaceToScope(
      section: Section,
      round: Int,
      raceToByScope: Map[String, Int]
  ): Either[String, Int] =
    resolveRoundRaceTo(section, round, raceToByScope) match {
      case Some(n) => Right(n)
      case None    => Left(scopeKey(section, round))
    }

  def resolveMatchRaceTo(
      matchDef: BracketMatch,
      raceToByScope: Map[String, Int]
  ): Option[Int] =
    matchDef.raceTo.orElse(scopeRaceTo(matchDef.id, raceToByScope))

  def matchRaceToScope(
      matchDef: BracketMatch,
      raceToByScope: Map[String, Int]
  ): Either[String, Int] =
    resolveMatchRaceTo(matchDef, raceToByScope) match {
      case Some(n) => Right(n)
      case None    =>
        val scope =
          RaceToScopes.keyForMatch(matchDef.id).getOrElse(matchDef.id)
        Left(scope)
    }

  def resultLabel(matchDef: BracketMatch): Option[String] =
    if (matchDef.isBye && matchDef.state == BracketMatchState.Completed) {
      Some("bye")
    } else if (
      matchDef.state == BracketMatchState.Completed && matchDef.forfeit.nonEmpty
    ) {
      matchDef.forfeit.map(info => s"forfeit: ${info.reason}")
    } else {
      matchDef.result.map(result => s"${result.scoreA}–${result.scoreB}")
    }

  enum MatchWinnerSide {
    case A
    case B
  }

  /** Winning side for completed non-bye matches with a clear score or forfeit
    * winner.
    */
  def winnerSide(matchDef: BracketMatch): Option[MatchWinnerSide] =
    Option
      .when(
        matchDef.state == BracketMatchState.Completed && !matchDef.isBye
      )(
        matchDef.forfeit
          .flatMap { info =>
            MatchSide.parse(info.forfeitingSide).map { forfeiting =>
              MatchSide.select(
                MatchSide.winnerFromForfeiting(forfeiting),
                MatchWinnerSide.A,
                MatchWinnerSide.B
              )
            }
          }
          .orElse(
            matchDef.result.flatMap { result =>
              if (result.scoreA > result.scoreB) Some(MatchWinnerSide.A)
              else if (result.scoreB > result.scoreA) Some(MatchWinnerSide.B)
              else None
            }
          )
      )
      .flatten

  enum AppliedHandicapSide {
    case PlayerA
    case PlayerB
  }

  /** Bracket/panel display for applied handicap: gated off, placed, or bug. */
  enum AppliedHandicapDisplay {
    case Hidden
    case Placed(spot: Int, side: AppliedHandicapSide, weakerName: String)
    case Unresolved(spot: Int)
  }

  /** Whether an applied-handicap side indicator should show on bracket rows. */
  def showsAppliedHandicap(matchDef: BracketMatch): Boolean = {
    val liveOrDone =
      matchDef.state == BracketMatchState.Started ||
        matchDef.state == BracketMatchState.Completed
    liveOrDone && matchDef.handicapApplied.exists(_ > 0)
  }
}
