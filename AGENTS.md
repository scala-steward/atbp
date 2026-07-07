# Agent Rules

## Scala tooling

### Default: always `sbt --client`

Use `sbt --client` for **every** sbt invocation unless one of the exceptions
below applies. This includes `compile`, `test`, `testOnly`, `fixup`, and
project-scoped tasks — not only `fixup`.

```bash
sbt --client compile
sbt --client "liga/test"
sbt --client "liga/testOnly *handicap*"
sbt --client fixup
```

**Do not use `sbt --batch` for routine compile/test work.** Common wrong
reasons that do **not** justify `--batch`:

- "I'm not sure the sbt server is running" — use `--client` anyway; it starts
  one if needed.
- "This is a one-off command" — still `--client`.
- "Only source files changed" — still `--client`.
- `git status` shows unrelated local changes under `project/` from a previous
  session — still `--client` unless **you** edited build files in this task.

### Exception: `sbt --batch` after build-definition edits

Use `sbt --batch` **only when you have changed** `*.sbt` files or files under
`project/` **in the current task** (e.g. `build.sbt`, `project/plugins.sbt`,
`project/Dependencies.scala`). A clean tree or stale uncommitted build edits
you did not touch does not switch the default.

```bash
# After editing build.sbt or project/*
sbt --batch compile
sbt --batch fixup
```

### Exception: `sbt run` (no `--client`)

To verify that the app starts, use `sbt run` **without** `--client` — the client
mode prevents interrupting the process.

### Commit workflow (required — never skip)

Before **every** commit that touches Scala (or `build.sbt` / `project/`):

1. Run `sbt --client fixup` on the whole project. No arguments. Not just changed files.
2. Run `git status` — if **any** file is modified, stage it and go back to step 1.
3. Repeat until `fixup` succeeds **and** `git status` shows no unstaged changes.
4. `git add` **all** source changes **and** any files modified by `fixup`.
5. Commit once — the commit must contain both the logic change and the `fixup` output.

**Never commit while `git status` still lists modified files after `fixup`.** A
successful `fixup` exit is not enough: compile can trigger another scalafmt pass, so
formatting changes may appear only on the next run.

**Never** commit until step 5 is done. Running `fixup` once and committing without
completing the loop is a workflow violation — amend or add a follow-up commit if that
happens.

`fixup` results must land in the **same** commit as the change they format, not in a
later commit.

Verify with this loop (do not skip `git status`):

```bash
sbt --client fixup && git status
```

If `git status` lists modified files, `git add` them, run the command again, and repeat
until `git status` is clean. Only then stage everything and commit.

In user-facing verification or commit summaries, do **not** claim `fixup` is clean unless
the last iteration of this loop left `git status` clean.
