"""Bounded local-worker output collection, AFTER the process tree stopped.

Only changed/deleted selected inputs and regular out/ files can be returned.
This does not acknowledge shutdown, import a result, certify it or merge it.
The execution controller must first establish actual process-tree quiescence.
"""
import base64
import hashlib
import os
import stat

import worker_inputs as inputs


MAX_ENTRIES = 2048


def _regular(info):
    if (not stat.S_ISREG(info.st_mode) or info.st_nlink != 1
            or info.st_mode & (stat.S_ISUID | stat.S_ISGID)
            or info.st_size > inputs.POLICY["max-file-bytes"]):
        raise ValueError("Worker output must be a bounded unlinked regular file")


def _read_file(directory, name):
    # Reject devices without opening them: even an O_NONBLOCK open can have
    # effects. Quiescence is required; post-open checks still catch replacement.
    inspected = os.stat(name, dir_fd=directory, follow_symlinks=False)
    _regular(inspected)
    # O_NONBLOCK makes a FIFO/device fail validation instead of hanging open.
    fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=directory)
    try:
        before = os.fstat(fd)
        _regular(before)
        if (inspected.st_dev, inspected.st_ino) != (before.st_dev, before.st_ino):
            raise ValueError("Worker output was replaced during collection")
        with os.fdopen(fd, "rb", closefd=False) as stream:
            data = stream.read(inputs.POLICY["max-file-bytes"] + 1)
        after = os.fstat(fd)
        if (len(data) != before.st_size or len(data) > inputs.POLICY["max-file-bytes"]
                or (before.st_size, before.st_mtime_ns, before.st_ctime_ns)
                != (after.st_size, after.st_mtime_ns, after.st_ctime_ns)):
            raise ValueError("Worker output changed during collection")
        return {"base64": base64.b64encode(data).decode(), "bytes": len(data),
                "sha256": hashlib.sha256(data).hexdigest(),
                "mode": 0o100755 if before.st_mode & 0o111 else 0o100644}
    finally:
        os.close(fd)


def _selected(root, path):
    directory = os.dup(root)
    try:
        parts = inputs.path_parts(path)
        for name in parts[:-1]:
            child = os.open(name, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=directory)
            os.close(directory)
            directory = child
        return _read_file(directory, parts[-1])
    except FileNotFoundError:
        return None
    finally:
        os.close(directory)


def collect(root, frozen, config, summary, exit_code):
    inputs.validate(frozen, config)
    if (not isinstance(summary, str) or len(summary) > 16000
            or type(exit_code) is not int or not 0 <= exit_code <= 255):
        raise ValueError("Invalid worker completion metadata")
    summary.encode("utf-8")
    root_fd = os.open(root, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
    files, total = {}, 0

    def add(path, file):
        nonlocal total
        # A file under out/ may also be an explicit selected input.
        if path in files:
            return
        if len(files) >= inputs.POLICY["max-files"]:
            raise ValueError("Too many worker result files/deletions")
        total += file["bytes"] if file else 0
        if total > inputs.POLICY["max-total-bytes"]:
            raise ValueError("Worker output exceeds total byte limit")
        files[path] = file

    entries = 0

    def visit(directory, prefix):
        nonlocal entries
        with os.scandir(directory) as children:
            for entry in children:
                entries += 1
                if entries > MAX_ENTRIES:
                    raise ValueError("Worker output directory exceeds traversal limit")
                path = prefix + "/" + entry.name
                inputs.path_parts(path)
                info = entry.stat(follow_symlinks=False)
                if stat.S_ISDIR(info.st_mode):
                    child = os.open(entry.name, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=directory)
                    try:
                        visit(child, path)
                    finally:
                        os.close(child)
                else:
                    result = _read_file(directory, entry.name)
                    if result != frozen["files"].get(path):
                        add(path, result)

    try:
        info = os.fstat(root_fd)
        if info.st_uid != os.getuid() or info.st_mode & 0o077:
            raise ValueError("Worker workspace must remain private and owned by you")
        for path, original in frozen["files"].items():
            current = _selected(root_fd, path)
            if current != original:
                add(path, current)
        try:
            out = os.open("out", os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=root_fd)
        except FileNotFoundError:
            out = None
        except NotADirectoryError:
            # A selected regular file literally named `out` is task input, not
            # the deliverable directory. _selected has already verified it.
            if "out" not in frozen["files"]:
                raise
            out = None
        if out is not None:
            try:
                visit(out, "out")
            finally:
                os.close(out)
        return {"files": files, "summary": summary, "exit-code": exit_code}
    finally:
        os.close(root_fd)
