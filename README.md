# lispore

Lispore runs one interactive shell inside a macOS PTY.
The MVP targets SBCL and CFFI.

## Install

The project uses ocicl for dependency installation.

```sh
ocicl install
```

## Run

The passthrough frontend runs from the repository root.

```sh
sbcl --noinform --load init --load examples/interactive.lisp
```

Passthrough mode forwards terminal bytes without interpretation.
It preserves ANSI control sequences and UTF-8 text.

This command starts the emulated frontend.

```sh
sbcl --noinform --load init \
  --eval '(asdf:load-system "lispore")' \
  --eval '(lispore:interactive-shell :mode :emulated)'
```

Emulated mode renders a screen grid.
It handles common ANSI cursor, erase, and style sequences.

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

Frontend modes restore terminal settings after normal exit and errors.

## Scope

The MVP excludes panes, detaching, reattachment, scrollback, mouse input, and windows.
