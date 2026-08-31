# Use four Lispore data operations

Lispore exposes `new`, `set`, `get`, and `del` through `lispore.api`.
`new` creates only a missing position, while `set`, `get`, and `del` require an existing position.
`nil` means missing, so a position cannot contain `nil`.
Session positions retain live managed Session objects rather than replacing them with lists.
The CLI mirrors the interface with `new session s1`, `set debug 1`, `get debug`, `del session s1`, and `set current-session s1`.
`current-session` is a control position that `set` initializes or switches.
Debug starts at `0` and is a protected position that `del` cannot remove.

## Status

accepted

## Considered Options

Lispore does not shadow `CL:SET` or `CL:GET`.
Those functions have different signatures and storage semantics.
The property-list implementation may still use `(setf (cl:get ...))`, `cl:get`, and `remprop` internally.

## Consequences

The new interface replaces the named-session and `debug` command forms described by ADR-0006 and ADR-0007.
Natural shell termination still retains a closed Session until normal retention expiry.
`del` terminates a live Session and removes its registry position immediately.
