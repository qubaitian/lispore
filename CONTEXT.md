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

**Session manager**:
A registry that owns shell sessions and their attachments.
It is the entry point for listing and selecting sessions.
_Avoid_: session service, session daemon

**Session ID**:
An opaque identifier for one managed shell session.
_Avoid_: session name, process ID

**Session name**:
A human-readable label used to select one shell session.
_Avoid_: session ID, process ID

**Session list**:
The available session names and execution states shown by the session manager.
_Avoid_: session picker, session menu

**Execution state**:
The status of a session's current submission.
`ready` means no active execution; `running` means one active execution.
`error` means the most recent submission failed; later input remains possible.
`closed` means the session no longer accepts input.
_Avoid_: process state

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
Input draft held by one terminal frontend before submission.
_Avoid_: shared input

**Input cursor**:
The position within frontend input where editing occurs.
_Avoid_: screen cursor

**Input submission**:
Complete input draft sent for Common Lisp evaluation or Shell execution.
_Avoid_: shared input

**Submission readiness**:
A state where the input is complete, the cursor is at its end, and no execution is active.
Only ready input can be submitted.
_Avoid_: submit-ready

**Input history**:
Complete input submissions retained by one shell session.
It includes successful and failed Common Lisp forms and shell commands.
_Avoid_: command history

**Draft history**:
Unsubmitted input drafts retained by one attachment for error recovery.
Boundary directional keys switch between retained drafts.
_Avoid_: input history

**Error draft**:
The input content from the most recent failed submission.
The command frontend restores it for editing.
_Avoid_: failed command

**Command frontend**:
A terminal frontend that accepts input drafts and routes them to Common Lisp or a shell session.
_Avoid_: input box

**Common Lisp form**:
A complete Common Lisp expression submitted through the command frontend.
_Avoid_: Lisp command

**Shell command**:
A complete shell-language input draft submitted to the shell session.
_Avoid_: shell input

**Incomplete input**:
An input draft that cannot be submitted because its selected language is incomplete.
Pressing Enter adds a newline to the draft.
_Avoid_: continuation prompt

**Active execution**:
A submitted Common Lisp form or shell command that has not finished.
The command frontend accepts editing but no new submission during active execution.
_Avoid_: busy state

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
A display component that turns terminal output into screen cells.
_Avoid_: terminal renderer

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

## Diagnostics

**Debug mode**:
A mode that exposes Lispore internal events and failures during session use.
_Avoid_: debugger mode

**Diagnostic log**:
A complete record of Lispore events, submitted input, and error reports.
_Avoid_: shell output
