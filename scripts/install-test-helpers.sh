#!/usr/bin/env bash
# CI only: callers bound this whole step as well as apt's individual requests.
set -euo pipefail

missing=()
command -v ffmpeg >/dev/null 2>&1 || missing+=(ffmpeg)
command -v python3 >/dev/null 2>&1 || missing+=(python3)

if ((${#missing[@]})); then
  apt_options=(-o Acquire::Retries=2 -o Acquire::http::Timeout=30 -o Acquire::https::Timeout=30)
  sudo apt-get "${apt_options[@]}" update
  sudo apt-get "${apt_options[@]}" install -y --no-install-recommends "${missing[@]}"
fi

# Presence is not enough if an installed helper cannot actually start.
ffmpeg -version >/dev/null
python3 --version
