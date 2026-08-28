# Clingon and Binary Delivery

This note explains Clingon, ASDF, Lisp runtimes, and executable delivery.
It uses primary sources from the projects and tools involved.
It checks Clingon `master` at commit [`e7e936b`](https://github.com/dnaeon/clingon/commit/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522).

## Findings

Clingon is a Common Lisp command-line options parser.
[The official README](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/README.org#L1-L32) states this role.

Clingon does not own binary packaging.
Its core ASDF system defines parser components and dependencies.
[The core ASDF file](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/clingon.asd#L30-L61) shows this scope.

Application systems define executable output and startup.
Clingon's demo system depends on Clingon and sets executable options.
[The demo ASDF file](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/clingon.demo.asd#L30-L58) shows this separation.

This role separation is an inference from the cited source layout.
Clingon defines command behavior.
ASDF or another builder creates the executable.
[The Clingon packaging guide](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/README.org#L742-L798) supports this conclusion.

## ASDF build path

The demo system declares `:build-operation "program-op"`.
It declares `:build-pathname "bin/clingon-demo"`.
It declares `:entry-point "clingon.demo:main"`.
[The demo ASDF file](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/clingon.demo.asd#L41-L58) defines these values.

ASDF uses the declared build operation for `asdf:make`.
[The ASDF grammar](https://asdf.common-lisp.dev/asdf/The-defsystem-grammar.html#Build-operation) defines this behavior.

ASDF `program-op` creates an executable from a system and dependencies.
ASDF accepts an entry point through `:entry-point`.
[The ASDF operation guide](https://asdf.common-lisp.dev/asdf/Predefined-operations-of-ASDF.html#Predefined-operations-of-ASDF) defines these behaviors.
[The ASDF entry-point definition](https://asdf.common-lisp.dev/asdf/The-defsystem-grammar.html#Entry-point) defines this option.

The Makefile loads the application with Quicklisp.
It then invokes `asdf:make`.
[The Clingon Makefile](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/Makefile#L7-L10) shows this sequence.

Quicklisp downloads and loads libraries.
ASDF compiles and loads those libraries.
[The Quicklisp FAQ](https://www.quicklisp.org/beta/faq.html#How-is-Quicklisp-related-to-ASDF) documents this division.

Quicklisp supports the build environment.
It does not provide the executable startup function.

## Runtime and packaging tools

SBCL can create standalone executables.
Those executables contain the SBCL runtime and Lisp image.
[The SBCL executable guide](https://www.sbcl.org/manual/#Generating-Executables) states this behavior.

SBCL combines the runtime and saved image when `:executable` is true.
[The `save-lisp-and-die` documentation](https://www.sbcl.org/manual/#Saving-a-Core-Image) defines this option.

ECL provides an implementation-specific `asdf:make-build` extension.
The extension comes with ECL's bundled ASDF.
[The ECL ASDF guide](https://ecl.common-lisp.dev/static/manual/System-building.html#Compiling-with-ASDF) states this limitation.

Clingon's ECL example selects `:type :program`.
It executes `clingon.demo:main` as epilogue code.
[The Clingon ECL example](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/README.org#L209-L227) shows this path.

ECL compiles Lisp files into objects first.
It then links those objects into an executable.
[The ECL executable guide](https://ecl.common-lisp.dev/static/manual/System-building.html#Executable) defines this process.

Buildapp is another application image builder.
It supports SBCL and CCL.
[The Buildapp README](https://github.com/xach/buildapp) states this purpose.

Buildapp loads systems and saves an executable image.
It does not parse application command lines.
[The Buildapp documentation](https://www.xach.com/lisp/buildapp/#Limitations) states these boundaries.

Clingon's Buildapp example loads `clingon.demo`.
It selects an application entry function.
[The Clingon Buildapp section](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/README.org#L1768-L1809) shows this usage.

## Why direct execution works

The build flow is:

```text
Quicklisp -> ASDF -> Clingon and application -> program-op -> executable
```

The run flow is:

```text
Executable -> Lisp runtime -> main -> clingon:run -> command handler
```

The demo `main` function builds the command tree.
It then calls `clingon:run`.
[The demo source](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/examples/demo/main.lisp#L70-L88) shows this flow.

`clingon:run` reads process arguments when callers provide none.
It parses arguments and invokes the selected handler.
It exits through UIOP after handling the command.
[The `run` implementation](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/src/command.lisp#L871-L881) shows these steps.

Clingon's `argv` function reads UIOP command-line arguments.
It uses ECL's raw arguments on ECL.
[The `argv` implementation](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/src/utils.lisp#L65-L70) shows this portability branch.

Clingon adds default `--help` and `--version` options.
[The Clingon README](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/README.org#L12-L23) documents these defaults.

The packager selects the executable startup function.
Clingon selects how that function handles arguments.
This explains direct `--help` execution after building.
[The Clingon README](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/README.org#L226-L239) demonstrates direct execution.

Direct source execution is a different path.
`sbcl --script file.lisp` asks SBCL to load the source file.
It does not prove that a standalone binary exists.
[The SBCL script guide](https://www.sbcl.org/manual/#Shebang-Scripts) describes this mode.

## Current lispore repository

The current repository does not depend on Clingon.
Its ASDF system depends on CFFI instead.
[The current system definition](../../lispore.asd) shows this dependency.

The current README starts SBCL and loads Lisp files.
It does not start a packaged application executable.
[The current README](../../README.md#run) shows these commands.

The `init` file can load the OCICL runtime.
It then configures ASDF's source registry.
[The current init file](../../init) shows this setup.

Therefore, current direct execution means source execution inside SBCL.
It does not mean native binary delivery.
The current system has no `:build-operation` or `:entry-point`.
[The current system definition](../../lispore.asd) contains no executable settings.

## Delivery limits

Standalone does not mean platform independent.
ASDF requires implementation support for `program-op`.
[The ASDF operation guide](https://asdf.common-lisp.dev/asdf/Predefined-operations-of-ASDF.html#Predefined-operations-of-ASDF) states this dependence.

SBCL core images have no binary compatibility across runtime support programs.
Build and run targets must match their runtime environment.
[The SBCL manual](https://www.sbcl.org/manual/#Saving-a-Core-Image) documents this limitation.

Foreign libraries and external resources may need separate distribution.
[The SBCL shared-object guide](https://www.sbcl.org/manual/#Loading-Shared-Object-Files) describes shared-library handling.

## Sources

- [Clingon README](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/README.org)
- [Clingon core ASDF system](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/clingon.asd)
- [Clingon demo ASDF system](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/clingon.demo.asd)
- [Clingon command source](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/src/command.lisp)
- [Clingon argument source](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/src/utils.lisp)
- [Clingon intro ASDF system](https://github.com/dnaeon/clingon/blob/e7e936b150ecd3e8a76795e0ba61fc6ddf67b522/clingon.intro.asd)
- [ASDF manual](https://asdf.common-lisp.dev/asdf.html)
- [SBCL manual](https://www.sbcl.org/manual/)
- [ECL manual](https://ecl.common-lisp.dev/static/manual/System-building.html)
- [Buildapp documentation](https://www.xach.com/lisp/buildapp/)
- [Quicklisp FAQ](https://www.quicklisp.org/beta/faq.html)
