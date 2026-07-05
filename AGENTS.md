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
3. `git add` **all** source changes **and** any files modified by `fixup`
4. Commit once — the commit must contain both the logic change and the `fixup` output

**Never** commit until step 3 is done. Running `fixup` and then committing without
staging its results is a workflow violation — amend or add a follow-up commit if that
happens.

`fixup` results must land in the **same** commit as the change they format, not in a
later commit.
