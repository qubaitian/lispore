# Lispore Terminal

This context defines shell sessions and terminal display behavior.
It does not define implementation details.

## Process

**Shell session**:
A running interactive shell with a controlling terminal.
_Avoid_: command, terminal process

**Current shell**:
The shell named by the user's `$SHELL` setting.
_Avoid_: zsh

**PTY**:
A pseudo-terminal pair that gives a child process terminal behavior.
_Avoid_: pipe

**PTY master**:
The endpoint that the Lisp process uses for session input and output.
_Avoid_: shell terminal

**PTY slave**:
The endpoint that the shell uses as its controlling terminal.
_Avoid_: child pipe

**Session termination**:
The end of a shell session after natural exit or explicit termination.
_Avoid_: detachment

## Display

**Terminal frontend**:
A component that connects a shell session to a terminal display.
_Avoid_: shell session

**Frontend input**:
Input held by one terminal frontend before submission to a shell session.
_Avoid_: shared input

**Input submission**:
Input sent by a terminal frontend to a shell session.
_Avoid_: shared input

**Shared output**:
Output from a shell session that every attached terminal frontend can observe.
_Avoid_: frontend-local output

**Attachment**:
A connection between a terminal frontend and a shell session.
_Avoid_: terminal session

**Detachment**:
The state where a terminal frontend disconnects while the shell session continues.
_Avoid_: session close, termination

**Reattachment**:
A new attachment to an existing running shell session.
_Avoid_: session restart

**Retained display**:
Screen contents that remain available after detachment or session termination.
_Avoid_: cleared screen

**Passthrough mode**:
A frontend that forwards terminal input and output without interpretation.
_Avoid_: raw mode

**Terminal emulator**:
A frontend that turns terminal output into screen cells.
_Avoid_: terminal renderer

**Emulated mode**:
A frontend that uses a terminal emulator and renders its screen cells.
_Avoid_: passthrough mode

**Screen grid**:
A rectangular display made of character cells and text styles.
_Avoid_: terminal buffer

**Status line**:
A fixed bottom row that shows shell session information.
_Avoid_: status bar

**Terminal size**:
The screen width and height measured in character cells.
_Avoid_: pixel size

## Text

**UTF-8 terminal text**:
User-visible terminal text encoded with UTF-8.
_Avoid_: ASCII text

**ANSI control sequence**:
A byte sequence that changes terminal display or cursor state.
_Avoid_: escape string
