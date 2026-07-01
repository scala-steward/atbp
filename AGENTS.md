# Agent Rules

## Scala tooling

- if you do need to use `sbt`, use `sbt --client` instead of `sbt` to connect to
  a running sbt server for faster execution
- to verify that the app starts use `sbt run`, WITHOUT `--client`, as it
  prevents interrupting the process
- before committing, ALWAYS format all changed Scala files using `fixup` task : `sbt --client fixup`
- ALWAYS include `fixup` results in the current commit so they don't show up as 
  part of the commit of next changes
