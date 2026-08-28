# Reserve the bottom row for the status line

Each terminal frontend reserves the bottom row for a fixed status line.
The shell uses the remaining rows so output cannot overwrite that line.

## Status

accepted

## Considered Options

Overlay the status line on the shell display.
This lets shell output overwrite the status line.

## Consequences

Both frontend modes show the same status line.
The PTY receives one fewer row than the terminal display.
