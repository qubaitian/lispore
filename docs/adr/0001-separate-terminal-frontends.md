# Separate terminal frontends

The MVP provides passthrough and emulated frontends over one shell session.
Passthrough forwards terminal input and output to the current terminal.
Emulated mode interprets UTF-8 text and common ANSI control sequences.

## Status

accepted

## Consequences

The shell session stays separate from terminal display behavior.
The terminal emulator supports a small ANSI control sequence subset.
Future multiplexing features can reuse the shell session layer.
The MVP excludes panes, detaching, scrollback, mouse input, and windows.
