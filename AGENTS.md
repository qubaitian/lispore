Read `CONTEXT.md` before exploring.
Read the ADRs in `docs/adr/` that touch the work.
Name domain concepts with that glossary.
Name an ADR conflict before you override it.

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

## Tests

Run tests with `--non-interactive` to keep the debugger off.

```sh
sbcl --noinform --non-interactive --load path/to/test.lisp
```
