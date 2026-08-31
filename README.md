# lispore

Lispore runs one interactive shell inside a macOS PTY.
The MVP targets SBCL and CFFI.

## Install

The project uses ocicl for dependency installation.

```sh
ocicl install
```

## Build

Build the standalone command from the repository root.

```sh
sbcl --noinform --load init \
  --eval '(asdf:make "lispore/app")' \
  --quit
```

The build writes the executable to `bin/lispore`.

## Run

Run the command without arguments from the repository root.

```sh
./bin/lispore
```

The command starts the background session manager when needed.
It prints the existing named sessions and their states.
It does not attach to a session.
Pass a session name to create or attach to that session.

```sh
./bin/lispore s1
```

The named command creates `s1` when it does not exist.
It reuses `s1` when it already exists.
The manager stays alive after the command exits.
The command provides `--help` and `--version` through Clingon.

Passthrough mode forwards terminal bytes without interpretation.
It preserves ANSI control sequences and UTF-8 text.
Managed frontends reserve the bottom row for a status line.
The command frontend routes Lisp forms and shell commands.
The status line shows the session and execution state.

This example starts the passthrough frontend directly.

```sh
sbcl --noinform --load init \
  --eval '(asdf:load-system "lispore")' \
  --eval '(lispore:run-passthrough)'
```

## Session interface

`start-shell` creates a shell session.
`read-output` and `write-input` handle UTF-8 terminal text.
`read-output-bytes` provides passthrough byte forwarding.
`resize-session` accepts character-cell dimensions.
`pty-master` returns the PTY master descriptor.
`close-session` belongs inside cleanup code.

```lisp
(let ((session (lispore:start-shell)))
  (unwind-protect
       (progn
         (lispore:write-input
          session
          (format nil "printf 'hello\\n'; exit~%"))
         (loop for text = (lispore:read-output session)
               while text
               do (write-string text)))
    (lispore:close-session session)))
```

The command and passthrough frontends restore terminal settings.

## Managed sessions

`make-session-manager` creates an in-process session registry.
`start-session` creates a fixed-size shell and returns an opaque ID.
`find-or-create-session` finds or creates one named session atomically.
`lookup-session-by-name` finds a named session.
`session-list` returns named sessions and their display states.
`start-session` uses the command frontend by default.
`attach-session` creates a frontend attachment for a running session.
`attach-session` uses the command frontend by default.
Use `:mode :command` for the command frontend.
Set `:mode :passthrough` when using the passthrough frontend.
`restore-session` reattaches and returns the retained terminal screen.
`reattach-session` provides the same operation with domain terminology.
`detach` removes one attachment without closing the shell session.
`read-attachment` reads output broadcast to that attachment.
`set-input-draft` stores input privately for one attachment.
`submit-input` submits one draft without interleaving concurrent input.
`submit-command` evaluates Lisp or runs shell commands serially.
`terminate-session` ends a managed session and prevents restoration.
`session-error` returns a background reader error after termination.
`close-session-manager` terminates its sessions during application cleanup.

The session manager retains final screens for a fixed time.
The CLI manager keeps sessions in its background process.
The manager scope is the current user on the current machine.
The bare command lists sessions and exits.
The named command creates or attaches to one named session.
Detachment does not survive command exit.
Existing attachments keep their final screen after termination.
Natural shell exit rejects every new attachment.
Manager restart does not preserve in-memory sessions.

## Scope

The MVP excludes panes, scrollback, mouse input, and windows.
