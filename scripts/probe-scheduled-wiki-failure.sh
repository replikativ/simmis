#!/usr/bin/env bash
# Opt-in failed candidate proof; scripted HTTP 200 + response.failed SSE.
set -euo pipefail
cd "$(dirname "$0")/.."
probe_root=$(mktemp -d /tmp/simmis-wiki-restart.XXXXXX)
mkdir -p "$probe_root/home" "$probe_root/codex" "$probe_root/claude"
printf 'Private failure evidence: %s\n' "$probe_root"
env -u TELEGRAM_BOT_TOKEN -u OPENAI_API_KEY -u ANTHROPIC_API_KEY \
  -u FIREWORKS_API_KEY -u CLAUDE_CODE_OAUTH_TOKEN -u CLAUDE_CODE_SETUP_TOKEN \
  DVERGR_HOME="$probe_root/dvergr" SIMMIS_DATA_DIR="$probe_root/data" \
  CODEX_HOME="$probe_root/codex" CLAUDE_CONFIG_DIR="$probe_root/claude" \
  SIMMIS_MAIL_SYNC=false \
  clj -J-Xmx1200m -J-XX:+UseSerialGC -J-Dspindel.missing-state=throw \
  "-J-Duser.home=$probe_root/home" \
  -Sdeps '{:aliases {:probe {:extra-paths ["test"]}}}' \
  -M:probe -m is.simm.ops.scheduled-wiki-failure-probe "$probe_root/failure.edn"
