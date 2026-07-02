# Liga

## Problem Statement

**How might we replace manual billiards standings and ad-hoc handicaps with reproducible Glicko2 ratings and a director-run handicapped double-elimination tournament — on one laptop, with a separate URL for the club TV?**

## Recommended Direction

**Files-first domain, single-process serve, split director/audience surfaces.**

The CLI (`atbp liga`) is the source-of-truth engine: read ordered period files, recompute Glicko2 ratings, print leaderboards, and calculate handicap suggestions for arbitrary matchups. Period files store match results as final scores only (e.g. `7–4`); game order within a match does not matter — the score expands to the correct win/loss counts for Glicko2. Presentation metadata (format name, race-to-N label) rides along for display but does not affect rating or ranking math.

**Rating periods.** A tournament is one Glicko2 rating period. Ratings are frozen at period start (carried forward from prior completed periods) and do not update as match results come in. Glicko2 runs only when a period completes and its period file is finalized. Bracket seeding and all handicap suggestions within a tournament use this same frozen snapshot.

**Race-to-N.** The director sets a race-to-N per bracket round (e.g. early rounds to 5, later rounds to 7). Per-match override is available as an escape hatch when a specific matchup needs a different race. Handicap suggestions use the effective N for that match: match override if set, otherwise the round default.

**Handicaps as suggestions.** When the director sets a match ready to play, the system shows a suggested handicap derived from period-start ratings and the match's effective race-to-N (75% cap). The director and players may accept it or adjust manually before play begins. The agreed handicap is locked once the match starts and stored on the match record (`handicap_suggested` vs `handicap_applied`). Game results drive Glicko2 updates at period end; handicaps are match metadata, not rating inputs. The CLI `liga handicap` mode uses the same suggestion algorithm for ad-hoc lookups.

**Players** are identified by display-name strings. Typos and renames are corrected via find-and-replace on period files — no separate ID layer in v1.

`atbp liga serve` runs a single JVM on the director's laptop. The director UI (Laminar SPA) manages a double-elimination bracket seeded by period-start ratings, with per-round race-to-N, per-match handicap suggestions, and manual override.

**In-progress persistence** is an append-only directory of JSON files — one directory per tournament. Each state change adds a new JSON file; existing files are never updated. Recording a match result appends a result file. On startup or after a crash, tournament state is reconstructed by replaying the directory in order. On tournament completion, the app compiles a finalized period file in CLI input format. Completed period files (the CLI ledger) are separate from in-progress tournament directories and are never mutated.

The audience gets a **separate URL** (e.g. `/audience`) opened in a second browser window on an external display. It is a read-only view of bracket progress and results — polling the same backend, no WebSockets, no shared SPA shell with the director.

## Key Assumptions to Validate

- [ ] **Handicap suggestions are usually close enough** — directors override rarely, not every match; spot-check with `liga handicap` on known pairings.
- [ ] **75% cap is sensible** — stress-test extreme rating gaps; confirm no absurd spots.
- [ ] **Override UX is fast** — adjusting a spot at the table takes seconds, not a workflow.
- [ ] **External display workflow** — director can open `/audience` on second screen without fuss.
- [ ] **16–32 bracket UX** — director can run a full event without getting lost in the tree.
- [ ] **Display-name strings are sufficient** — find-and-replace on period files handles typos and renames without an ID system.

## MVP Scope

**In:**

- `liga` CLI: read period file(s) or directory; leaderboard (rating, RD, W–L); handicap suggestion mode (two players + race-to-N).
- Period file format: player display names, matches with final scores, effective race-to-N, `handicap_suggested` / `handicap_applied`, optional presentation metadata.
- Glicko2 engine: game outcomes derived from final scores; ratings advance only at period boundaries; reproducible replay across periods in order.
- `liga serve`: ZIO HTTP backend; Laminar director UI; double-elim bracket; rating-seeded draw; per-round race-to-N with per-match override; suggested handicaps with manual override; match result entry; append-only JSON directory persistence + crash recovery.
- Audience route at separate URL: read-only bracket/standings, poll-based refresh.
- Emit finalized period file on tournament completion.

**Out (v1):**

- Single-elimination and round-robin formats.
- Authentication / user accounts.
- Multiple concurrent tournaments.
- Cloud / hosted deployment.
- Mobile apps.
- Editing past period files through the web app.
- Format-aware rating logic (metadata is display-only).
- Per-rack game sequence in period files.
- In-period rating updates (mid-tournament rating drift).
- Stable player IDs (display names + find-and-replace instead).

## Not Doing (and Why)

- **WebSockets / live push for audience** — polling on a club TV is fine; halves complexity.
- **Unified SPA with director + audience tabs** — separate URL matches real setup (laptop + external display).
- **Format in rating/ranking math** — mixed formats are a presentation concern; Glicko2 stays format-agnostic.
- **Game-order within matches** — final score carries all information Glicko2 needs.
- **Non-tournament period sources** — v1 ratings advance via tournament completion only; casual play can come later.
- **Auth** — single director on localhost; no multi-user editing surface.
- **Rigid auto-applied handicaps** — suggestions only; humans have final say at match setup.
- **Handicap edits after play starts** — agreed spot is locked once the match begins.
- **In-period rating updates** — Glicko2 runs at period boundaries only, not per match.
- **Player ID system** — display names + find-and-replace on files is enough for one club.
- **Mutable state files** — append-only JSON avoids corruption on crash; no in-place updates.
- **SQLite for tournament state** — directory replay is simpler and matches the files-first philosophy.

## Open Questions

_(None — ready for spec.)_

## Resolved Decisions

| Question | Decision |
|----------|----------|
| MVP scope | Full stack: CLI + director + audience |
| Period granularity | One tournament = one rating period |
| Result storage | Final score only (e.g. `7–4`), not per-rack sequence |
| Rating updates | Period boundaries only; frozen during tournament |
| Handicaps | Suggested at match setup; director/players accept or override |
| Audience view | Separate URL on external display; poll-based, not part of director SPA |
| Format metadata | Presentation only; not used in rating/ranking |
| Player identity | Display-name strings; find-and-replace on period files for corrections |
| Serve runtime | Single JVM on director's laptop |
| Race-to-N | Per bracket round; per-match override as escape hatch |
| Tournament persistence | Append-only directory of JSON files, one dir per tournament; no in-place updates; replay on resume |
