#!/usr/bin/env bash
# Run sbt fixup before git commit and stage any formatting changes so they
# land in the same commit. Mirrors the loop in AGENTS.md.
set -euo pipefail

input=$(cat)
command=$(printf '%s' "$input" | jq -r '.command // empty')

# Hooks may not load shell profiles; add common sbt install locations.
export PATH="${HOME}/.local/share/coursier/bin:${HOME}/Library/Application Support/Coursier/bin:${PATH}"

if [[ ! "$command" =~ git[[:space:]]+commit ]]; then
  echo '{ "permission": "allow" }'
  exit 0
fi

# Do not block informational or dry-run invocations.
if [[ "$command" =~ (^|[[:space:]])--dry-run([[:space:]]|$) ]] \
  || [[ "$command" =~ (^|[[:space:]])-n([[:space:]]|$) ]] \
  || [[ "$command" =~ (^|[[:space:]])--help([[:space:]]|$) ]] \
  || [[ "$command" =~ (^|[[:space:]])-h([[:space:]]|$) ]]; then
  echo '{ "permission": "allow" }'
  exit 0
fi

if ! command -v sbt >/dev/null 2>&1; then
  echo '{
    "permission": "deny",
    "user_message": "sbt is not on PATH. Install sbt or add it to PATH before committing.",
    "agent_message": "The pre-commit fixup hook could not find `sbt`. Ensure sbt is installed and available on PATH, then retry the commit."
  }'
  exit 2
fi

max_iterations=10
iteration=0
staged_fixup=false

while (( iteration < max_iterations )); do
  iteration=$((iteration + 1))

  if ! sbt --client fixup; then
    echo '{
      "permission": "deny",
      "user_message": "sbt fixup failed. Fix errors before committing.",
      "agent_message": "The pre-commit fixup hook blocked this commit because `sbt --client fixup` failed. Resolve the failures, stage changes, then commit again."
    }'
    exit 2
  fi

  if git diff --quiet; then
    break
  fi

  git add -u
  staged_fixup=true
done

if ! git diff --quiet; then
  echo '{
    "permission": "deny",
    "user_message": "fixup did not stabilize after multiple iterations. Run `sbt --client fixup` manually until `git status` is clean.",
    "agent_message": "The pre-commit fixup hook stopped after too many iterations with unstaged changes remaining. Run `sbt --client fixup`, stage all changes, verify `git status` is clean, then commit."
  }'
  exit 2
fi

if [[ "$staged_fixup" == true ]]; then
  echo '{
    "permission": "allow",
    "agent_message": "Pre-commit hook ran `sbt --client fixup` and staged formatting changes. Proceeding with commit."
  }'
else
  echo '{
    "permission": "allow",
    "agent_message": "Pre-commit hook ran `sbt --client fixup`; no additional staging was needed. Proceeding with commit."
  }'
fi
exit 0
