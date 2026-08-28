# Manage detachable PTY sessions in process

ADR-0001 excludes detachment from the MVP.
This decision supersedes that scope exclusion.
The low-level shell session still stays separate from display behavior.
Managed sessions own shared display state for attached frontends.
An in-process session manager owns each PTY and its background reader.
It stores opaque IDs and retains final screens for a fixed time.
Detachment removes one attachment without terminating the shell session.
Reattachment accepts only running sessions and restores their retained screen.

## Status

accepted

## Consequences

PTY output broadcasts to every attached frontend.
Each attachment owns a bounded output buffer.
Buffer overflow disconnects only the slow attachment.
Input drafts stay private to their attachments.
The session size stays fixed after creation.
Service restart loses all in-memory sessions.
