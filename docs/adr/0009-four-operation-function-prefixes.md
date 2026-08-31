# Use four operation prefixes for Lispore functions

Lispore names stateful functions with `new`, `set`, `get`, or `del` prefixes.
This removes competing verbs for the same data operation.

## Status

accepted

## Considered Options

Semantic verbs such as `start`, `attach`, and `lookup` are clearer alone.
The four prefixes reduce the vocabulary that callers must learn.

## Consequences

The rename changes every Lispore caller, test, and public export.
Lispore provides no aliases for the old function names.
Noun and predicate names remain when they do not describe operations.
