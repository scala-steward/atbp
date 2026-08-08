package ph.samson.atbp.liga.bracket

import ph.samson.atbp.liga.model.*
import zio.test.*

object CutReseedSpec extends ZIOSpecDefault {

  private def rating(player: Player): PlayerRating =
    PlayerRating(player, rating = 1500, rd = 100, wins = 0, losses = 0)

  private def findMatch(bracket: Bracket, id: String): BracketMatch =
    bracket.matches.find(_.id == id).get

  /** LB forfeit that eliminates `loser` without changing earned racks. */
  private def lbForfeit(
      id: String,
      winner: Player,
      loser: Player
  ): BracketMatch =
    BracketMatch(
      id = id,
      playerA = Some(winner),
      playerB = Some(loser),
      state = BracketMatchState.Completed,
      forfeit = Some(
        MatchForfeitInfo(forfeitingSide = "B", reason = "cut-test")
      )
    )

  def spec = suite("CutReseed")(
    test(
      "reseed tie-breaks equal racks and ratings by roster draw order"
    ) {
      // Roster order (draw): Zeb first. Seed/name order would put Amy first.
      val zeb = Player("Zeb")
      val amy = Player("Amy")
      val carl = Player("Carl")
      val dana = Player("Dana")
      val eve = Player("Eve")
      val fay = Player("Fay")
      val gus = Player("Gus")
      val hal = Player("Hal")
      val roster = List(zeb, amy, carl, dana, eve, fay, gus, hal)
      val survivors = List(zeb, amy, carl, dana)
      val context = CutReseed.Context(
        players = roster,
        frozenRatings = roster.map(p => p -> rating(p)).toMap
      )
      val bracket = Bracket(
        size = 8,
        topN = 4,
        matches = List(
          lbForfeit("lb-1-1", zeb, eve),
          lbForfeit("lb-1-2", amy, fay),
          lbForfeit("lb-1-3", carl, gus),
          lbForfeit("lb-1-4", dana, hal),
          BracketMatch(
            id = "se-1-1",
            playerA = None,
            playerB = None,
            state = BracketMatchState.Pending
          ),
          BracketMatch(
            id = "se-1-2",
            playerA = None,
            playerB = None,
            state = BracketMatchState.Pending
          ),
          BracketMatch(
            id = "se-2-1",
            playerA = None,
            playerB = None,
            state = BracketMatchState.Pending
          )
        )
      )

      val reseeded = CutReseed.reseed(bracket, context).toOption.get
      val se11 = findMatch(reseeded, "se-1-1")
      val se12 = findMatch(reseeded, "se-1-2")
      // Ranked by draw order Zeb, Amy, Carl, Dana → SE seats Zeb vs Dana, Amy vs Carl
      assertTrue(
        CutReseed.survivors(bracket, roster) == survivors,
        se11.playerA.contains(zeb),
        se11.playerB.contains(dana),
        se12.playerA.contains(amy),
        se12.playerB.contains(carl)
      )
    }
  )
}
