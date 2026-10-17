"""Bounded nonblocking local pipe frames with an absolute monotonic deadline."""
import json
import os
import selectors
import time

import worker_inputs as inputs

MAX_FRAME_BYTES = inputs.POLICY["max-input-json-bytes"] + 262144


class Frames:
    def __init__(self, writer, reader, tick=lambda: None):
        self.writer, self.reader, self.tick = writer, reader, tick
        self.pending = bytearray()
        os.set_blocking(writer, False)
        os.set_blocking(reader, False)

    def _ready(self, fd, event, deadline):
        self.tick()
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise TimeoutError("Worker pipe protocol deadline exceeded")
        with selectors.DefaultSelector() as selector:
            selector.register(fd, event)
            # A real lease/deadline service point, never a timing workaround.
            return selector.select(timeout=min(remaining, 1))

    def write(self, encoded, deadline):
        if not isinstance(encoded, bytes) or len(encoded) > MAX_FRAME_BYTES:
            raise ValueError("Worker pipe request exceeds its bound")
        offset = 0
        while offset < len(encoded):
            if self._ready(self.writer, selectors.EVENT_WRITE, deadline):
                try:
                    offset += os.write(self.writer, encoded[offset:offset + 16384])
                except BlockingIOError:
                    pass

    def read(self, deadline):
        while True:
            self.tick()
            if time.monotonic() >= deadline:
                raise TimeoutError("Worker pipe protocol deadline exceeded")
            newline = self.pending.find(b"\n")
            if newline >= 0:
                encoded = bytes(self.pending[:newline + 1])
                del self.pending[:newline + 1]
                value = json.loads(encoded.decode("utf-8"), object_pairs_hook=inputs._unique_object,
                                   parse_constant=inputs._invalid_constant)
                if type(value) is not dict:
                    raise ValueError("Worker pipe frame must be an object")
                return value
            if len(self.pending) >= MAX_FRAME_BYTES:
                raise ValueError("Worker pipe frame exceeds its bound")
            if self._ready(self.reader, selectors.EVENT_READ, deadline):
                try:
                    chunk = os.read(self.reader, min(16384, MAX_FRAME_BYTES - len(self.pending)))
                except BlockingIOError:
                    continue
                if not chunk:
                    raise EOFError("Worker pipe exited without its complete bounded frame")
                self.pending.extend(chunk)

    def close(self):
        for attribute in ["writer", "reader"]:
            fd = getattr(self, attribute)
            if fd is not None:
                os.close(fd)
                setattr(self, attribute, None)
