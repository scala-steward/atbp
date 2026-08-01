package ph.samson.atbp.liga.js.director

import zio.test.*

object RosterSoftRemoveSpec extends ZIOSpecDefault {

  def spec = suite("RosterSoftRemove")(
    test("activeNames preserves order and excludes removed") {
      val names = List("Alice", "Bob", "Carol")
      val removed = Set("Bob")
      assertTrue(
        RosterSoftRemove.activeNames(names, removed) == List("Alice", "Carol")
      )
    },
    test("toggle adds then removes a name") {
      val removed0 = Set.empty[String]
      val removed1 = RosterSoftRemove.toggle(removed0, "Bob")
      val removed2 = RosterSoftRemove.toggle(removed1, "Bob")
      assertTrue(
        removed1 == Set("Bob"),
        removed2 == Set.empty[String]
      )
    },
    test("toggle accepts names not on the roster") {
      assertTrue(
        RosterSoftRemove.toggle(Set.empty, "Unknown") == Set("Unknown")
      )
    },
    test("activeNames is the Save/Lock payload filter") {
      val names = List("Alice", "Bob", "Carol")
      val removed = Set("Alice", "Carol")
      assertTrue(
        RosterSoftRemove.activeNames(names, removed) == List("Bob")
      )
    },
    test("commitNames is Save/Lock payload and cannot see paste") {
      // Signature takes only roster + removes — Lock must not parse paste.
      val names = List("Alice", "Bob", "Carol")
      val removed = Set("Bob")
      assertTrue(
        RosterSoftRemove.commitNames(names, removed) == List("Alice", "Carol"),
        RosterSoftRemove.commitNames(names, removed) ==
          RosterSoftRemove.activeNames(names, removed)
      )
    },
    test("guestBadgeText always occupies the badge grid column") {
      assertTrue(
        RosterSoftRemove.guestBadgeText(isGuest = true) == "guest",
        RosterSoftRemove.guestBadgeText(isGuest = false) == ""
      )
    }
  )
}
