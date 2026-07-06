# Agent Rules

## Scala tooling

- if you do need to use `sbt`, use `sbt --batch --client` instead of `sbt` to connect to
  a running sbt server for faster execution
- to verify that the app starts use `sbt --batch run`, WITHOUT `--client`, as it
  prevents interrupting the process
- before committing, ALWAYS format all changed Scala files using `fixup` task : `sbt --batch --client fixup`
