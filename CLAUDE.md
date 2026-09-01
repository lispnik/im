# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

CFFI bindings to [IM](https://www.tecgraf.puc-rio.br/im/), Tecgraf's C imaging
toolkit, plus `im(1)`, a clingon command-line tool over them. Unaffiliated with
Tecgraf. Built against [lispnik/tecgraf-im](https://github.com/lispnik/tecgraf-im),
a CMake fork of IM 3.15 whose local checkout is usually at
`~/Projects/tecgraf/tecgraf-im`.

## Commands

```sh
ocicl install                        # restore pinned dependencies
make                                 # build bin/im
make test                            # asdf:test-system :im
make bindings IM_SOURCE=/path/to/tecgraf-im   # regenerate src/ffi/

# One suite, or one test
sbcl --non-interactive --eval '(asdf:load-system :im/tests)' \
     --eval "(5am:run! 'im.tests::process-suite)"
```

The Makefile uses a hermetic source registry (`:ignore-inherited-configuration`),
so a missing dependency fails loudly rather than resolving to a neighbouring
checkout. There is no dependency on any sibling project — `tecgraf-base` was
dropped, and everything else comes from ocicl.

Set `IM_LIBRARY_PATH` to a directory of IM shared libraries to test against a
specific build; otherwise Homebrew's `tecgraf-im` is usually found.

## Architecture

### Four systems in one `im.asd`

| System | Contents |
|---|---|
| `im` | The binding: `src/ffi/` (raw) plus `src/*.lisp` (the Lisp API) |
| `im/cli` | `im(1)`. `program-op` → `bin/im`, entry point `im.cli:main` |
| `im/tests` | FiveAM suite |

`im/cli`'s components live in a `:module` rather than under a system-level
`:pathname`, because ASDF resolves `:build-pathname` against the system's
pathname — with `:pathname "src/cli"` the binary lands in `src/cli/bin/im`.

### Two packages

`im.ffi` holds the raw bindings — `%im-file-open` for `imFileOpen`, C types,
nothing exported. `im` holds everything public and reaches in with double
colons. Operation families are namespaced by *symbol name* (`im:convolve-sobel`,
`im:morph-erode`), not by package; the previous version's twenty flat packages
are gone.

### `src/ffi/` is generated

`tools/gen-bindings.lisp` drafts it from the IM headers and is then
hand-corrected in place. Two things matter:

- **Never put a hand-written file in `src/ffi/`.** Regenerating clears the
  directory. Hand-written FFI code goes in `src/ffi-structs.lisp`.
- **The symbol list comes from `nm` on the built libraries**, not from the
  headers. Headers declare functions no library implements — that is how the
  previous binding ended up with four entry points that existed until you
  called them. `src/ffi/manifest.lisp` records every binding, and a test checks
  at runtime that each one resolves.

Parsing traps already found and fixed, all of which failed *silently*:

- Comments and function-like `#define`s must be blanked before scanning, or a
  parenthesis in prose bridges into the next real declaration and swallows it.
  Ten functions vanished this way.
- `IM_DECL` (in `im_capture.h`) must be blanked too, or the scan restarts at it
  and captures it as the return type. All 27 capture functions were `:pointer`.
- `(string-trim " \t" ...)` in Common Lisp trims space **and the letter t** —
  there is no `\t` escape in CL string literals. Every C type lost a trailing
  `t`, so `format` was bound as `forma`.

### Resource ownership

`im:image` wraps `imImage` with three overlapping safeguards: `with-image`
releases on unwind, a finalizer catches escapees, and `destroy` is idempotent
and disarms the finalizer so the two cannot double-free. The finalizer closes
over a cons cell holding the pointer, **never over the image** — a finalizer
that references its own object keeps it alive forever.

### Cancellation and restarts

`im:with-progress` installs a callback; returning false cancels. Cancelling
signals `im:operation-aborted` and establishes `retry` and `continue` restarts.
`define-process-op` wraps every processing function in this, so the protocol is
uniform rather than applied where someone remembered.

The previous design used a CFFI `:wrapper` return type whose translator called
`signal` on a non-`error` condition, so a cancelled operation returned NIL and
execution continued into a half-written destination image.

### Library loading

`src/library.lisp` handles discovery (`IM_LIBRARY_PATH`, a `lib/` beside the
executable, then CFFI's own search) and, critically, image dump/restore hooks.
CFFI's record of an open library survives `save-lisp-and-die`, so without a
restore hook `use-foreign-library` short-circuits in the dumped binary and
never calls `dlopen` — leaving the process bound to whatever the loader found.

Three pairs of libraries export the same symbols and must not both be open:
`im_process` vs `im_process_omp` (identical sets), and `im_fftw3`, which
redefines six of `im_process`'s. The FFT wrappers resolve their pointers
against a named library through `im:fft-symbol` rather than letting `dlsym`
order decide.

## Things that are easy to get wrong

- **`imStats` has `unsigned long` fields** — 8 bytes on LP64, 4 on Windows
  LLP64. `:unsigned-long` is the only spelling that is right on both;
  `:uint64` shifts `mean` and `stddev` by eight bytes on Windows.
- **Region labelling needs a gray *ushort* destination.** IM documents this and
  does not check; an int image gives plausible, wrong answers. Use
  `im:make-label-image`.
- **`imAnalyzeMeasureCentroid` writes `double*`**, not float.
- **`imFormatCanWriteImage` returns an error code**, so zero means yes.
- **Compression cannot be set as an image attribute.** IM ignores it; `im:save`
  uses `imFileNew`/`imFileSetInfo` instead. This is easy to miss because TIFF's
  default is LZW, so asking for LZW appears to work.
- **`imProcessIFFT` needs both images complex** and segfaults otherwise, so the
  wrappers check preconditions IM documents but does not enforce.
- **`~v,0T` divides by zero** on SBCL when colinc is 0.
- **An attribute a format does not recognise is dropped silently.** PNG stores
  `"Author"`, TIFF does not — it has no tag for it and says nothing. Read the
  file back rather than trusting the write.
- **SWANK prints wire messages in its own `swank-io-package`.** An Emacs image
  spec needs the symbol `png`, so `im:display` interns it there; the obvious
  `:png` arrives as a keyword, `find-image` compares with `memq`, and the
  image silently never renders.
- **SBCL's floating-point traps are not on everywhere.** `(coerce 1d300
  'single-float)` signals `floating-point-overflow` on macOS arm64, Linux
  amd64 and Windows, and returns an infinity on Linux arm64. Range-check
  against `most-positive-single-float` rather than catching the trap —
  CI is the only place the difference shows up.
- **Comparing against a NaN is itself a trap.** A range check placed before
  the `handler-case` that catches arithmetic errors is the error path, not a
  guard against it. NaNs reach attribute code by the ordinary route: read a
  float attribute from a file, set it on another image.
- **A cleanup sweep must key on what it wrote, not on what the name looks
  like.** `im:display` numbers files from a counter that restarts each
  process, so matching `image-<n>.png` in a user-nominated directory deleted
  files it had never written. Files carry a per-process token now.
- **`find-symbol` signals when its package designator names nothing**, unlike
  `find-package`. Probing for an optional package (`swank`, `slynk`) must
  lead with `find-package`, or the probe is the crash.
- **SLY's `eval-in-emacs` must be sent with `nowait`.** Emacs checks
  `sly-enable-evaluate-in-emacs` in the event dispatcher, outside the handler
  that turns an error into a return value, so the blocking form waits forever
  when the option is at its default.

## Tests

`tests/`, one file per area, 224 checks. Beyond the obvious coverage they
assert the things that previously went untested: the condition hierarchy, the
restart protocol, finalizer and double-destroy behaviour, that every binding
resolves against the loaded libraries, and — by running `bin/im` as a
subprocess — that the dumped image reopens its libraries.

`tests/display.lisp` binds `im:*display-function*` in every test. Without it,
running the suite from inside a SLY session would send image buffers to the
person running it.

The `test-op` deliberately errors when `fiveam:run!` returns NIL. Discarding
that value is how this project's CI once went green with three failing tests.

Sample images are in `tests/images/`. `.gitattributes` marks image extensions
`binary`; without it the `ocicl/* text eol=lf` rule above strips the CR from a
PNG signature and the fixture stops being a PNG.
