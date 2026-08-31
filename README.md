# lispore

Lispore runs one interactive shell inside a macOS PTY.
The MVP targets SBCL and CFFI.

## Requirements

Use a Common Lisp implementation.
Current verification uses SBCL.
Use [OCICL](https://github.com/ocicl/ocicl) for dependencies.
Legacy OpenSSL may need `USE_LEGACY_OPENSSL=1`.
Then run `sbcl --load setup.lisp`.

## Build

Build the standalone command from the repository root.

```sh
sbcl --noinform --load init \
  --eval '(asdf:make "lispore/app")' \
  --quit
```

The build writes the executable to `bin/lispore`.

## Run

The CLI uses four data operations.
The operation, path, and value use separate arguments.

```sh
./bin/lispore new session s1
./bin/lispore set debug 1
./bin/lispore get debug
./bin/lispore del session s1
./bin/lispore set current-session s1
```

`new session s1` creates a named Session without entering it.
`set current-session s1` enters or switches to an existing Session.
`del session s1` terminates the Session and removes its registry position.
`get session` queries the named Session list.
Invalid operations report errors and return a non-zero status.
The manager stays alive after each command exits.

```sh
./bin/lispore get session
```

Debug mode uses an existing value position.
The value `1` enables Debug mode.
The value `0` disables Debug mode.
Debug starts at `0` and cannot be deleted.

```sh
./bin/lispore set debug 1
./bin/lispore set debug 0
./bin/lispore get debug
```

Debug mode keeps normal terminal output.
It also shows Lispore diagnostic records in active sessions.
The log includes submitted input, errors, and SBCL backtraces.
SBCL errors do not enter the interactive debugger.

The command provides `--help` and `--version` through Clingon.

## Lisp interface

The public Lisp interface lives in `lispore.api`.

```lisp
(lispore.api:new :session "s1")
(lispore.api:set :debug 1)
(lispore.api:get :debug)
(lispore.api:set :current-session "s1")
(lispore.api:del :session "s1")
```
