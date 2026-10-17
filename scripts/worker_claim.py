"""Private, durable claim identity and exclusive local-controller ownership.

Persist Run + nonce + proof BEFORE sending a claim. The nonce is the lease ID,
so even a lost reply followed by revocation retains minimal shutdown authority.
This module neither claims work nor authorizes execution. A controller must
obtain a successful live claim and reconcile any old process tree separately.
The caller must durably create the private state directory (including syncing
its entry in its parent) before admission. Never remove/recreate lock files
while controllers could still own them.
"""
import contextlib
import fcntl
import json
import os
from pathlib import Path
import re
import secrets
import stat
import uuid

import worker_inputs as inputs


MAX_TICKET_BYTES = 2048


def _directory(path):
    fd = os.open(path, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
    info = os.fstat(fd)
    if info.st_uid != os.getuid() or info.st_mode & 0o077:
        os.close(fd)
        raise ValueError("Worker state directory must be private and owned by you")
    return fd


def _regular_info(info):
    if (not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid()
            or stat.S_IMODE(info.st_mode) != 0o600):
        raise ValueError("Worker state file must be owned by you with mode 600")


def _validate(ticket):
    if (type(ticket) is not dict or set(ticket) != {"version", "worker", "run", "nonce", "proof"}
            or type(ticket["version"]) is not int or ticket["version"] != 1
            or not isinstance(ticket["proof"], str) or not re.fullmatch(r"[A-Za-z0-9_-]{43}", ticket["proof"])):
        raise ValueError("Invalid private claim ticket")
    for key in ("worker", "run", "nonce"):
        inputs.canonical_uuid(ticket[key])
    return ticket


def _unique(pairs):
    out = {}
    for key, value in pairs:
        if key in out:
            raise ValueError("Duplicate private claim field")
        out[key] = value
    return out


def _load(directory, name):
    inspected = os.stat(name, dir_fd=directory, follow_symlinks=False)
    _regular_info(inspected)
    fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=directory)
    try:
        current = os.fstat(fd)
        _regular_info(current)
        if (current.st_dev, current.st_ino) != (inspected.st_dev, inspected.st_ino):
            raise ValueError("Private claim ticket changed during open")
        with os.fdopen(fd, "rb", closefd=False) as stream:
            encoded = stream.read(MAX_TICKET_BYTES + 1)
        if len(encoded) > MAX_TICKET_BYTES:
            raise ValueError("Private claim ticket exceeds its limit")
        try:
            ticket = json.loads(encoded.decode("utf-8"), object_pairs_hook=_unique)
        except (ValueError, UnicodeError, RecursionError):
            raise ValueError("Malformed private claim ticket") from None
        return _validate(ticket)
    finally:
        os.close(fd)


def load(path):
    """Inspect a ticket, not a durable admission barrier; use prepare to claim."""
    path = Path(path)
    directory = _directory(path.parent)
    try:
        return _load(directory, path.name)
    finally:
        os.close(directory)


def prepare(path, worker, run):
    """Durably publish one immutable ticket; concurrent exact retries share it.

    An existing ticket is never replaced, even if corrupt or for another Run.
    Successful return includes a directory fsync, not merely a file write.
    The caller must already have durably created the parent directory. Private
    crash-left .claim-* staging files are non-authoritative, not claim tickets.
    """
    inputs.canonical_uuid(worker)
    inputs.canonical_uuid(run)
    path = Path(path)
    directory = _directory(path.parent)
    staged = None
    try:
        try:
            ticket = _load(directory, path.name)
        except FileNotFoundError:
            ticket = {"version": 1, "worker": worker, "run": run,
                      "nonce": str(uuid.uuid4()), "proof": secrets.token_urlsafe(32)}
            staged = ".claim-" + uuid.uuid4().hex
            fd = os.open(staged, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                         0o600, dir_fd=directory)
            with os.fdopen(fd, "wb") as stream:
                # A restrictive umask may remove owner bits. Successful
                # preparation must remain readable on a later restart.
                os.fchmod(stream.fileno(), 0o600)
                _regular_info(os.fstat(stream.fileno()))
                stream.write(json.dumps(ticket, sort_keys=True).encode("utf-8"))
                stream.flush()
                os.fsync(stream.fileno())
            try:
                # Atomic no-overwrite publication. A loser adopts only the
                # exact winner's durable identity, never its own second proof.
                os.link(staged, path.name, src_dir_fd=directory, dst_dir_fd=directory, follow_symlinks=False)
            except FileExistsError:
                ticket = _load(directory, path.name)
        if ticket["worker"] != worker or ticket["run"] != run:
            raise ValueError("Claim ticket belongs to different work")
        if staged is not None:
            os.unlink(staged, dir_fd=directory)
            staged = None
        os.fsync(directory)
        return ticket
    finally:
        try:
            if staged is not None:
                os.unlink(staged, dir_fd=directory)
        finally:
            os.close(directory)


def lease_token(ticket):
    """Credential material; never print or pass as a process argument.

    A proposed bearer authenticates nobody before the exact claim commits.
    Knowing this string does not imply a successful claim or a stopped worker.
    """
    _validate(ticket)
    return "simmis-lease." + ticket["run"] + "." + ticket["nonce"] + "." + ticket["proof"]


@contextlib.contextmanager
def controller_lock(folder, worker):
    """Hold through network admission, process lifetime and physical cleanup.

    OS lock ownership is a physical controller resource, not application state.
    Process death releases the lock, NOT any orphaned unit or server lease;
    a replacement controller must reconcile those before executing new work.
    Lock files are permanent: unlinking/recreating one can split ownership
    across two inodes even while the original controller still holds its lock.
    """
    inputs.canonical_uuid(worker)
    directory = _directory(folder)
    fd = None
    try:
        name = worker + ".lock"
        try:
            _regular_info(os.stat(name, dir_fd=directory, follow_symlinks=False))
        except FileNotFoundError:
            pass
        fd = os.open(name, os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW | os.O_NONBLOCK,
                     0o600, dir_fd=directory)
        _regular_info(os.fstat(fd))
        try:
            fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            raise ValueError("Another controller owns this local worker") from None
        yield
    finally:
        try:
            if fd is not None:
                os.close(fd)
        finally:
            os.close(directory)
