Single-context: root `CONTEXT.md` + `docs/adr/`. See `docs/agents/domain.md`.

Be efficient.
Write the minimum code that works.
Prefer deletion over addition.
Write for a junior reviewer.
The code should be as easy to understand and review.
Code comments are necessary.
Use Conventional Commits.
Use Conventional Branch Names.

when you chat, reply, write docs, commit message:
    Use in ASD-STE100 Simplified Technical English.
    Use the active voice.
    Use the present tense.
    Use short sentence.
    Put one idea per sentence.
    Put one sentence per line.
    Limit sentences to 12 words.

## SBCL Test Commands

Run every SBCL test in non-interactive batch mode.

Use this command pattern:

```sh
sbcl --noinform --non-interactive --load path/to/test.lisp
```

The `--non-interactive` option combines `--quit` and `--disable-debugger`.

Exit immediately after the test completes.

On an unhandled error, print the error and backtrace.

Then exit with a non-zero status.

Use `sbcl --script path/to/test.lisp` for standalone test scripts.

Ensure the test runner reports failures through errors or exit codes.
