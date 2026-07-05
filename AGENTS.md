# Agent Rules

## Scala tooling

- if you do need to use `sbt`, use `sbt --client` instead of `sbt` to connect to
  a running sbt server for faster execution
- to verify that the app starts use `sbt run`, WITHOUT `--client`, as it
  prevents interrupting the process

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
