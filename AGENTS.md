# Agent Rules

## Scala tooling

- if you do need to use `sbt`, use `sbt --client` instead of `sbt` to connect to
  a running sbt server for faster execution
- to verify that the app starts use `sbt run`, WITHOUT `--client`, as it
  prevents interrupting the process

### Commit workflow (required — never skip)

Before **every** commit that touches Scala (or `build.sbt` / `project/`):

1. Run `sbt --client fixup` on the whole project. No arguments. Not just changed files.
2. Run `git status` / `git diff` and confirm there are no unstaged formatting changes
3. If step 2 shows changes, stage them and run `fixup` again; repeat until `git status` is clean
4. `git add` **all** source changes **and** any files modified by `fixup`
5. Commit once — the commit must contain both the logic change and the `fixup` output

**Never** commit until step 4 is done. Running `fixup` and then committing without
staging its results is a workflow violation — amend or add a follow-up commit if that
happens.

`fixup` results must land in the **same** commit as the change they format, not in a
later commit.

**Do not report "fixup is clean"** unless `fixup` exits successfully **and** `git status`
shows no unstaged changes afterward. A successful `fixup` exit alone is not enough —
scalafmt may still reformat files that must be staged before the commit is complete.

Verify with this loop (do not skip `git status`):

```bash
sbt --client fixup && git status
```

If `git status` lists modified files, stage them, run the command again, and repeat until
`git status` is clean. Only then stage everything and commit.

In user-facing verification or commit summaries, do **not** list `fixup` as passing unless
you ran this loop and `git status` was clean on the last iteration.
