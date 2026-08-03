# Intent: TD match wait indicators

Confirmed statement of intent (output of an interview-me session). This captures
*what* we want and *why*, not the implementation. The spec is downstream.

## Outcome

On the existing TD match list, show how long each Ready match has had a player
waiting, order Ready matches by longest waiting player first, and show how long
each Done match has been in that state.

## User

Tournament director running the floor.

## Why now

Without that signal the director guesses urgency and rest gaps by memory /
asking around.

## Success

At a live event the director can glance at Ready and always call the
longest-waiting match next, and check Done dwell before rushing the later
arriver into their next match.

## Constraint

Elapsed-time indicators and Ready ordering only — no thresholds, colors-as-rules,
or auto-calling.

**Wait clock**

- Starts when the first player becomes available for that match (e.g. their
  prior match went Done).
- For matches with no prior (e.g. first round), starts when the match becomes
  Ready.

**Done clock**

- Elapsed time since the match went Done; director eyeballs “enough” rest —
  no configured threshold.

## Out of scope

- New screens or queues
- Rest-time policies or auto-highlight thresholds
- Notifications
- Changing the match state machine beyond using existing availability / Ready /
  Done timing
