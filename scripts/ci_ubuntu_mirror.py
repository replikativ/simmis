#!/usr/bin/env python3
"""Use the canonical signed Ubuntu archive instead of the GH runner's Azure mirror.

Invoked explicitly by GitHub CI only; not application or developer setup.
"""

import argparse
from pathlib import Path
import re


# Runner images use either the direct Azure URI or this exact mirror-list URI.
# Do not rewrite arbitrary local lists, repository paths or signing fields.
RUNNER_URI = re.compile(
    r"(?<!\S)(?:http://azure\.archive\.ubuntu\.com/ubuntu/?|"
    r"mirror\+file:/etc/apt/apt-mirrors\.txt)(?!\S)")
URI_FIELD = re.compile(r"^([ \t]*URIs:[ \t]*)([^\r\n]*)", re.MULTILINE | re.IGNORECASE)


def canonical_archive(source):
    return URI_FIELD.sub(
        lambda field: field[1] + RUNNER_URI.sub("https://archive.ubuntu.com/ubuntu", field[2]),
        source,
    )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--sources-file", type=Path,
                        default=Path("/etc/apt/sources.list.d/ubuntu.sources"))
    path = parser.parse_args().sources_file
    if not path.is_file():
        print("Ubuntu deb822 sources not present; leaving runner repositories unchanged.")
        return
    original = path.read_bytes().decode("utf-8")
    updated = canonical_archive(original)
    if updated == original:
        print("No recognized Azure runner archive URI; leaving runner repositories unchanged.")
        return
    path.write_bytes(updated.encode("utf-8"))
    print("Selected canonical HTTPS Ubuntu archive; suites and signing configuration unchanged.")


if __name__ == "__main__":
    main()
