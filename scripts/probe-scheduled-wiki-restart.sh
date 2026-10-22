#!/usr/bin/env bash
# Opt-in, offline diagnostic. Does not start the web app or use a live home.
set -euo pipefail
cd "$(dirname "$0")/.."

probe_root=$(mktemp -d /tmp/simmis-wiki-restart.XXXXXX)
mkdir -p "$probe_root/home" "$probe_root/codex" "$probe_root/claude"
printf 'Private diagnostic evidence: %s\n' "$probe_root"

run_phase() {
  env -u TELEGRAM_BOT_TOKEN -u OPENAI_API_KEY -u ANTHROPIC_API_KEY \
    -u FIREWORKS_API_KEY -u CLAUDE_CODE_OAUTH_TOKEN -u CLAUDE_CODE_SETUP_TOKEN \
    DVERGR_HOME="$probe_root/dvergr" SIMMIS_DATA_DIR="$probe_root/data" \
    CODEX_HOME="$probe_root/codex" CLAUDE_CONFIG_DIR="$probe_root/claude" \
    SIMMIS_MAIL_SYNC=false \
    clj -J-Xmx1200m -J-XX:+UseSerialGC -J-Dspindel.missing-state=throw \
    "-J-Duser.home=$probe_root/home" \
    -Sdeps '{:aliases {:probe {:extra-paths ["test"]}}}' \
    -M:probe -m is.simm.ops.scheduled-wiki-restart-probe \
    "$1" "$probe_root/prepared.edn"
}

# Process B cannot start until A has physically exited successfully.
run_phase prepare
run_phase reopen
printf 'Both processes completed; evidence retained at %s\n' "$probe_root"
