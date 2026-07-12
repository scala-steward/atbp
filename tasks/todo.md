# Todo: Liga code-review remediation

> Plan: [plan.md](./plan.md) · Spec: [SPEC.md](../SPEC.md)

---

## Task 1: Fix `isLocalDirector` for missing `remoteAddress`

**Description:** Replace `remoteAddress.forall(_.isLoopbackAddress)` with `exists` so `None` is treated as non-local and write routes return 403.

**Acceptance criteria:**
- [x] `BindConfig.isLocalDirector` returns `false` when `remoteAddress` is `None`
- [x] Loopback and non-loopback behaviour unchanged
- [x] Existing `--lan` write-block test still passes

**Verification:**
- [x] `sbt --client "liga/testOnly *BindConfigSpec"`

**Dependencies:** None

**Files:**
- `liga/src/main/scala/ph/samson/atbp/liga/serve/BindConfig.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/BindConfigSpec.scala`

**Scope:** XS

---

## Task 2: Sanitize static JS asset paths

**Description:** Reject `fileName` values containing `..` or `/` before classpath lookup; return 404.

**Acceptance criteria:**
- [x] `/assets/js/../director.js` or segment `..` → 404
- [x] Valid `director.js` / `audience.js` still served

**Verification:**
- [x] `sbt --client "liga/testOnly *StaticAssetsSpec"`

**Dependencies:** None

**Files:**
- `liga/src/main/scala/ph/samson/atbp/liga/serve/StaticAssets.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/StaticAssetsSpec.scala`

**Scope:** XS

---

## Checkpoint A

- [x] Tasks 1–2 complete
- [x] `sbt --client "liga/testOnly *BindConfigSpec *StaticAssetsSpec"`

---

## Task 3: Reject duplicate roster players

**Description:** Add `DuplicatePlayersError` to `Tournament.setPlayers`; reject when `players.distinct.size != players.size` (case-sensitive).

**Acceptance criteria:**
- [x] `Tournament.setPlayers` returns `Left` for duplicate `Player("Alice")` entries
- [x] `"Alice"` and `"alice"` still allowed (distinct players)
- [x] `POST /api/tournament/players` returns 400 with clear message

**Verification:**
- [x] `sbt --client "liga/testOnly *TournamentSpec"`
- [x] `sbt --client "liga/testOnly *WriteApiSpec"` (new duplicate-players test)

**Dependencies:** None (can follow Checkpoint A)

**Files:**
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/Tournament.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/TournamentSpec.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/WriteApiSpec.scala`

**Scope:** S

---

## Task 4: Enforce handicap bounds server-side

**Description:** In `applyHandicap`, resolve race-to for match round and reject `handicap < 0` or `handicap > floor(0.75 * raceTo)`.

**Acceptance criteria:**
- [x] Negative handicap rejected
- [x] Handicap above cap rejected (e.g. race-to 7, cap 5, handicap 6 → Left)
- [x] Valid handicap still accepted
- [x] `POST /api/matches/{id}/handicap` returns 400 on violation

**Verification:**
- [x] `sbt --client "liga/testOnly *TournamentSpec"`
- [x] `sbt --client "liga/testOnly *WriteApiSpec"` (handicap bounds tests)

**Dependencies:** None

**Files:**
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/Tournament.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/TournamentSpec.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/WriteApiSpec.scala`

**Scope:** S

---

## Task 5: Enforce race-to score rules on result

**Description:** Extend `validateScores` to use `MatchLifecycle.resolveRaceTo`; winner score must equal race-to; loser score strictly less; keep tie/negative checks.

**Acceptance criteria:**
- [x] `scoreA: 999, scoreB: 0` rejected when race-to is 7
- [x] `7–4` accepted for race-to 7
- [x] `7–7` still rejected (tie)
- [x] `POST /api/matches/{id}/result` returns 400 on violation

**Verification:**
- [x] `sbt --client "liga/testOnly *TournamentSpec"`
- [x] `sbt --client "liga/testOnly *WriteApiSpec"` (invalid score tests)

**Dependencies:** None

**Files:**
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/Tournament.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/TournamentSpec.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/WriteApiSpec.scala`

**Scope:** S

---

## Checkpoint B

- [x] Tasks 3–5 complete
- [x] `sbt --client "liga/testOnly *TournamentSpec *WriteApiSpec *EndToEndSpec"`

---

## Task 6: Reorder `completeTournament` and map HTTP 409/500

**Description:** In `ServeContext.completeTournament`, call `PeriodEmission.write` before appending `TournamentCompleted`. Map existing file → 409, other write failures → 500. Ensure failed write leaves no completed event on disk.

**Acceptance criteria:**
- [x] Happy path: period file exists and `TournamentCompleted` event appended
- [x] Pre-existing period file: no new completed event; HTTP 409
- [x] `Replay.isComplete` false after failed complete attempt
- [x] `EndToEndSpec` leaderboard/period test still passes

**Verification:**
- [x] New test in `WriteApiSpec` or `ServeCheckpointSpec` (pre-create target `.liga`, POST complete → 409, assert no `*-completed.json`)
- [x] `sbt --client "liga/testOnly *EndToEndSpec"`

**Dependencies:** Tasks 3–5 recommended (valid match data in tests)

**Files:**
- `liga/src/main/scala/ph/samson/atbp/liga/serve/ServeContext.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/serve/DirectorRoutes.scala` (status mapping for emission errors)
- `liga/src/test/scala/ph/samson/atbp/liga/serve/WriteApiSpec.scala` or `ServeCheckpointSpec.scala`

**Scope:** M

---

## Checkpoint C

- [x] Task 6 complete
- [x] `sbt --client "liga/testOnly *EndToEndSpec *WriteApiSpec"`

---

## Task 7: Generic 500 responses

**Description:** Replace `Response.text(err.getMessage)` on unexpected errors in `DirectorRoutes` and read API (`Routes.scala`) with a fixed generic message.

**Acceptance criteria:**
- [x] Unexpected failures return 500 with body not containing exception class/message details
- [x] Expected 400/403/409 behaviour unchanged

**Verification:**
- [x] `sbt --client "liga/testOnly *WriteApiSpec *ReadApiSpec"`

**Dependencies:** Task 6 (409 mapping in place first)

**Files:**
- `liga/src/main/scala/ph/samson/atbp/liga/serve/DirectorRoutes.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/serve/Routes.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/WriteApiSpec.scala` (optional assertion)

**Scope:** S

---

## Task 8: Director error mapping and phase comment

**Description:** Add `DirectorGuidance.friendlyApiError` entries for new validation messages (duplicate players, handicap cap, winner score). Add brief comment on `TournamentPhase.derive` explaining `locked` vs `raceTo`.

**Acceptance criteria:**
- [x] New API error substrings mapped to director-friendly copy
- [x] Comment clarifies phase naming without changing API labels

**Verification:**
- [x] `sbt --client "liga-js/compile"` (if guidance is Scala.js)
- [x] Manual spot-check of error strings

**Dependencies:** Tasks 3–5 (error messages must exist)

**Files:**
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/DirectorGuidance.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/TournamentPhase.scala`

**Scope:** XS

---

## Checkpoint D — Complete

- [x] All tasks 1–8 done
- [x] `sbt --client "liga/test"`
- [x] `sbt --client fixup && git status` clean
- [x] All SPEC.md success criteria checked off
