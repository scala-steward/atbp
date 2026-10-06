#!/usr/bin/env bash
set -euo pipefail

# Use --batch only after changing this task's build definition in the current task.
sbt_mode=${1:---client}
case "$sbt_mode" in
  --client|--batch) ;;
  *) echo "Usage: $0 [--client|--batch]" >&2; exit 2 ;;
esac

repo=$(cd "$(dirname "$0")/.." && pwd -P)
fixture=$(mktemp -d /tmp/atbp-install.XXXXXX)
test_home="$fixture/home with spaces"
stage="$repo/cli/target/universal/stage"
stage_hold="${stage}.install-local-check.$$"
sbt_touched=false

cleanup() {
  local status=$?
  if [ -d "$stage_hold" ]; then
    mv "$stage_hold" "$stage"
  fi
  # Client settings survive command completion, including a failed stage task.
  # Reload before deleting the temporary home so the next install uses defaults.
  if [ "$sbt_mode" = --client ] && [ "$sbt_touched" = true ]; then
    if ! sbt "$sbt_mode" reload >"$fixture/cleanup.log" 2>&1; then
      cat "$fixture/cleanup.log" >&2
      status=1
    fi
  fi
  rm -rf "$fixture"
  exit "$status"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

mkdir -p "$test_home"
cd "$repo"

run_install() {
  local log=$1
  shift
  local commands="set localInstallHome := file(\"$test_home\")"
  local command
  for command in "$@"; do
    commands="$commands; $command"
  done
  sbt_touched=true
  sbt "$sbt_mode" "$commands; installLocal" >"$log" 2>&1
}

expect_success() {
  local name=$1
  shift
  if ! run_install "$fixture/$name.log" "$@"; then
    cat "$fixture/$name.log" >&2
    exit 1
  fi
}

expect_failure() {
  local name=$1
  local message=$2
  shift 2
  if run_install "$fixture/$name.log" "$@"; then
    echo "FAIL: $name unexpectedly succeeded" >&2
    exit 1
  fi
  if ! rg -q -F "$message" "$fixture/$name.log"; then
    cat "$fixture/$name.log" >&2
    echo "FAIL: $name did not report $message" >&2
    exit 1
  fi
}

# Load the current build before applying the temporary-home override.
sbt_touched=true
if [ "$sbt_mode" = --client ] && ! sbt "$sbt_mode" reload >"$fixture/reload.log" 2>&1; then
  cat "$fixture/reload.log" >&2
  exit 1
fi
expect_success install

installation="$test_home/.local/share/atbp"
launcher="$installation/bin/atbp"
command_link="$test_home/.local/bin/atbp"
test -x "$launcher"
test -L "$command_link"
test "$(readlink "$command_link")" = "$launcher"
diff -r "$stage" "$installation"
test "$(rg -c -F "Installed distribution: $installation" "$fixture/install.log")" = 1
test "$(rg -c -F "Installed command: $command_link" "$fixture/install.log")" = 1

# Restore staging on success, assertion failure, or interruption.
test ! -e "$stage_hold"
mv "$stage" "$stage_hold"
(cd "$fixture" && "$command_link" --help) >"$fixture/help.log" 2>&1
rg -q 'atbp' "$fixture/help.log"
mv "$stage_hold" "$stage"
echo "PASS: complete first install, paths with spaces, and help without staging"

# Upgrade an existing distribution and repair a link without touching its target.
former_target="$fixture/former command"
printf 'keep former command\n' >"$former_target"
cp "$former_target" "$fixture/former-command.expected"
rm "$command_link"
ln -s "$former_target" "$command_link"
printf 'stale packaged file\n' >"$installation/stale-file"
printf 'outdated launcher\n' >"$launcher"
expect_success reinstall
test ! -e "$installation/stale-file"
diff -r "$stage" "$installation"
test -x "$launcher"
test "$(readlink "$command_link")" = "$launcher"
cmp "$former_target" "$fixture/former-command.expected"

missing_target="$fixture/missing command"
rm "$command_link"
ln -s "$missing_target" "$command_link"
expect_success dangling-command
test "$(readlink "$command_link")" = "$launcher"
test ! -e "$missing_target"
(cd "$fixture" && "$command_link" --help) >"$fixture/reinstall-help.log" 2>&1
rg -q 'atbp' "$fixture/reinstall-help.log"
echo "PASS: reinstall replaces files, removes stale files, and repairs both command symlink types"

# Keep a full snapshot plus a file absent from stage to detect any replacement.
printf 'preserve previous distribution\n' >"$installation/preserve-on-failure"
cp -R "$installation" "$fixture/expected-installation"
rm "$command_link"
printf 'preserve ordinary command\n' >"$command_link"
cp "$command_link" "$fixture/command.expected"
expect_failure command-file "Command path must be absent or a symlink: $command_link"
cmp "$command_link" "$fixture/command.expected"
diff -r "$fixture/expected-installation" "$installation"

rm "$command_link"
mkdir "$command_link"
cp "$fixture/command.expected" "$command_link/preserve"
expect_failure command-directory "Command path must be absent or a symlink: $command_link"
test -d "$command_link"
cmp "$command_link/preserve" "$fixture/command.expected"
diff -r "$fixture/expected-installation" "$installation"
rm -r "$command_link"
ln -s "$launcher" "$command_link"
echo "PASS: ordinary command file and directory conflicts preserve command data and distribution"

mv "$installation" "$fixture/saved-installation"
installation_target="$fixture/former installation"
mkdir "$installation_target"
cp "$fixture/command.expected" "$installation_target/preserve"
ln -s "$installation_target" "$installation"
expect_failure installation-symlink "Installation path must be a directory, not a symlink or file: $installation"
test "$(readlink "$installation")" = "$installation_target"
cmp "$installation_target/preserve" "$fixture/command.expected"
test "$(readlink "$command_link")" = "$launcher"
diff -r "$fixture/expected-installation" "$fixture/saved-installation"

rm "$installation"
ln -s "$fixture/missing installation" "$installation"
expect_failure dangling-installation "Installation path must be a directory, not a symlink or file: $installation"
test "$(readlink "$installation")" = "$fixture/missing installation"
test ! -e "$fixture/missing installation"
test "$(readlink "$command_link")" = "$launcher"

rm "$installation"
cp "$fixture/command.expected" "$installation"
expect_failure installation-file "Installation path must be a directory, not a symlink or file: $installation"
cmp "$installation" "$fixture/command.expected"
test "$(readlink "$command_link")" = "$launcher"
diff -r "$fixture/expected-installation" "$fixture/saved-installation"
rm "$installation"
mv "$fixture/saved-installation" "$installation"
echo "PASS: installation symlinks and ordinary file conflicts preserve destinations and their data"

expect_failure staging-failure 'Injected staging failure' \
  'set cli / Universal / stage := sys.error("Injected staging failure")'
diff -r "$fixture/expected-installation" "$installation"
test "$(readlink "$command_link")" = "$launcher"
test -x "$launcher"
echo "PASS: staging failure preserves the complete distribution and command link"
