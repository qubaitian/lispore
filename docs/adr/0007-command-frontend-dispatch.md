# Add a command frontend

The application adds a command frontend over one persistent shell session.
It owns line input, evaluates lines beginning with `(` in Lispore, and sends other lines to the shell.
This keeps shell state while separating command entry from the PTY display.

## Status

accepted

## Consequences

The command frontend needs a line editor and a Common Lisp evaluator.
The application hides the shell prompt and command echo.
The screen needs separate output, input, and status rows.
Existing passthrough and emulated frontends remain available.
Lisp and shell drafts can span multiple lines when incomplete.
The command frontend does not submit new input during active execution.
The input area scrolls when its content reaches the available screen height.
The status line shows the session ID and execution state.
Reader errors preserve the current input draft for editing.
A non-whitespace character after a Lisp form selects Shell completeness rules.
The input editor supports cursor movement and directional keys.
Evaluation errors preserve the current input draft for editing.
A submission requires complete input with the cursor at its end.
Successful submissions clear the input and restore the output view.
Execution errors restore the input draft for editing.
The session retains every submitted input in input history.
Successful and failed submissions both enter input history.
Current unsubmitted drafts remain private to their attachments.
Editing recalled input creates a new history entry after submission.
Boundary directional keys navigate input history.
Input history remains in memory until session termination.
Attachments retain unsubmitted drafts in private draft history.
Recovery navigation uses draft history before normal navigation uses input history.
Detachment clears private draft history but keeps input history.
The command frontend becomes the application's default interactive frontend.
The PTY continues running one persistent current shell.
Common Lisp evaluation runs inside a persistent user package.
Returned values and Lisp output enter shared output.
One serialized execution worker serves each session.
The input editor supports Home, End, Delete, UTF-8, and multiline paste.
The input area can fill the screen above the status line.
Incomplete input turns Enter into a newline.
Complete input needs the cursor at the draft end.
Active execution rejects complete submissions without changing the draft.
Ctrl-C clears drafts or interrupts active execution.
Ctrl-D closes an empty command frontend and otherwise does nothing.
The shell completeness check supports common continuations and compound syntax.
Uncertain shell completeness still sends the draft to the shell.
Interactive programs continue using the passthrough frontend.
