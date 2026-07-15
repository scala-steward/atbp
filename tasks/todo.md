# Tasks: Roster UX remediation

- [x] Task: Enable ZIO Test on `ligaCommon.jvm` and add `RatingOrderSpec`
  - Acceptance: `sbt --client "ligaCommonJVM/testOnly *RatingOrder*"` passes
  - Verify: `ligaCommonJVM/testOnly *RatingOrder*`
  - Files: `build.sbt`, `liga-common/src/test/.../RatingOrderSpec.scala`

- [x] Task: Delegate `Seeding.seedOrder` to `RatingOrder`
  - Acceptance: No `compareRatings` in `Seeding.scala`; `BracketSpec` passes
  - Verify: `liga/testOnly *Bracket*`
  - Files: `liga/.../Seeding.scala`

- [x] Task: `RosterPaste` L1 bare-`\r` and L3 single-pass guest
  - Acceptance: New bare-`\r` test passes; existing `RosterPasteSpec` green
  - Verify: `liga/testOnly *RosterPaste*`
  - Files: `liga-common/.../RosterPaste.scala`, `liga/.../RosterPasteSpec.scala`

- [x] Task: Save-then-lock and hint coherence in director UI
  - Acceptance: Lock chains save+lock; dirty-paste hint; summary hints when clean
  - Verify: `liga-js/compile`, manual define step
  - Files: `DirectorGuidance.scala`, `DirectorApp.scala`, `WizardView.scala`
