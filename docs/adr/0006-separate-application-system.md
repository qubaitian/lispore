# Separate the application system from the library

The standalone `lispore` command uses a separate application system.
Its entry point ensures a process-local manager and creates one new session.
This keeps the reusable library independent from Clingon and executable packaging.

## Status

accepted

## Consequences

The command uses the passthrough frontend by default.
Detachment does not preserve sessions after application exit.
Cross-process session reuse remains outside this change.
