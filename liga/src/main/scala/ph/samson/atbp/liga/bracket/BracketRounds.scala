package ph.samson.atbp.liga.bracket

import ph.samson.atbp.liga.tournament.MatchLifecycle

import scala.collection.immutable.SortedSet

/** Topology-derived bracket round keys for race-to configuration. */
object BracketRounds {

  /** Distinct round integers that appear in match ids for this player count. */
  def requiredKeys(playerCount: Int): SortedSet[Int] = {
    val bracketSize = Seeding.bracketSize(playerCount)
    val topology = BracketTopology(bracketSize)
    topology.matches.keys
      .flatMap(MatchLifecycle.bracketRound(_, bracketSize))
      .to(SortedSet)
  }
}
