# im — Common Lisp bindings to the IM imaging toolkit

CFFI bindings to [IM](https://www.tecgraf.puc-rio.br/im/), Tecgraf's toolkit for
image representation, storage, capture and processing, plus `im(1)`, a command
line tool that drives them.

This project is unaffiliated with Tecgraf.

Built against [lispnik/tecgraf-im](https://github.com/lispnik/tecgraf-im), a
CMake fork of IM 3.15. The bindings cover **456 C functions** — every function
exported by `libim`, `libim_process`, `libim_capture`, `libim_fftw3` and the
format add-ons, apart from a documented list of driver internals.

## Installing

Prebuilt binaries for Linux (amd64 and arm64), macOS (Apple Silicon) and
Windows are attached to each [release](https://github.com/lispnik/im/releases).
They embed an SBCL core but do not bundle IM, so the shared libraries still
have to be present -- see *Finding the libraries* below.

## Building from source

- SBCL, and [ocicl](https://github.com/ocicl/ocicl) for dependencies
- IM 3.15 shared libraries

```sh
ocicl install          # restore the pinned dependencies
make                   # build bin/im
make test              # run the test suite
```

## Finding the libraries

The bindings look for `libim` and its add-ons in this order:

1. `im:*library-path*`, if set
2. the `IM_LIBRARY_PATH` environment variable
3. a `lib/` directory beside the running executable — the release layout
4. `cffi:*foreign-library-directories*` and the platform's own search path

```sh
IM_LIBRARY_PATH=/path/to/tecgraf-im/build/lib ./bin/im library
```

`im library` reports the IM version and exactly which shared objects were
opened, which is the fastest way to tell whether an add-on is present.

Add-ons are optional. `libim_jp2`, `libim_heif` and `libim_capture` are all
switched off in upstream's default build, and the bindings load without them —
their formats simply do not appear in `im formats`.

## The command line tool

```
im info FILE...        format, dimensions, colour mode, attributes
im formats             the registered formats and their compressions
im convert IN OUT      format, compression, colour space or depth
im process IN OUT      a pipeline of operations
im analyze FILE        label connected regions and measure them
im stats FILE          per-plane statistics and histograms
im compare A B         RMS error and signal-to-noise ratio
im capture             list capture devices, or grab a frame
im library             IM version and the libraries in use
```

Every subcommand takes `--json`, which emits one JSON value:

```sh
im info photo.jpg --json | jq '.frames[0] | {width, height}'
```

Operations in `im process` are given as repeated `--op` arguments and applied
**in the order written**, which is why they are not one flag each:

```sh
im process in.jpg out.png \
    --op resize=50% --op colorspace=gray --op gaussian=1.5 --op sobel
```

`im process --list-ops` lists all twenty. Sizes accept `WxH`, `800x` or `x600`
to preserve the aspect ratio, and `50%`.

Exit codes are 0 for success, 1 for an IM error, 2 for a usage error and 130
for an interrupt. Diagnostics go to stderr, so piping stdout to `jq` is safe.

## The library

```lisp
(asdf:load-system :im)

(im:with-image (photo (im:load #p"photo.jpg"))
  (im:with-image (edges (im:create-based photo :color-space :color-space-gray))
    (im:convolve-sobel photo edges)
    (im:save edges #p"edges.png")))
```

### Images

`im:image` is a CLOS object wrapping IM's `imImage`. Its storage is released by
`im:destroy`, by `im:with-image` on unwind, or — for images that escape both —
by a finalizer. `im:destroy` is idempotent and disarms the finalizer, so the
two cannot race. Operating on a destroyed image signals `im:invalid-image`
rather than reading freed memory.

Pixel data is reached through `im:plane-pointer`, a raw foreign pointer. That
is deliberate, and it is IM's own reasoning: the library supports so many data
organisations that general-purpose per-pixel accessors would be both
complicated and slow. Planes are always unpacked and stored bottom-up.

### Conditions

Every failure is a subtype of `im:im-error`, and each of IM's error codes has
its own class, so causes are distinguished by handler rather than by testing a
slot:

```lisp
(handler-case (im:load path)
  (im:open-error   (c) (format t "cannot open: ~A" (im:error-detail c)))
  (im:format-error (c) (format t "unrecognised format")))
```

### Progress and cancellation

A callback installed with `im:with-progress` is called as an operation runs and
can stop it. Cancelling signals `im:operation-aborted` — a real error, not a
silent NIL — and the operation offers `retry` and `continue` restarts:

```lisp
(im:with-progress ((lambda (id text percent)
                     (declare (ignore id text))
                     (< percent 500)))          ; stop halfway
  (im:convolve-gaussian source destination 8.0))
```

## Layout

| Path | Contents |
|---|---|
| `src/ffi/` | The raw bindings. Generated, then hand-corrected. **Do not add hand-written files here** — the generator clears it. |
| `src/ffi-structs.lisp` | Hand-written C structs, kept outside the generated directory. |
| `src/*.lisp` | The Lisp API: conditions, library loading, images, files, processing, capture. |
| `src/cli/` | `im(1)`, one file per subcommand group. |
| `tools/gen-bindings.lisp` | The binding generator. Not part of any shipped system. |

### Regenerating the bindings

```sh
make bindings IM_SOURCE=/path/to/tecgraf-im
```

The generator takes its symbol list from `nm` on the **built libraries**, not
from the headers, so it cannot bind a function that does not exist. Headers
declare several that no library implements. It also emits
`src/ffi/manifest.lisp`, which the test suite uses to check at runtime that
every bound function resolves.

Regenerating overwrites everything under `src/ffi/`. Run it into a clean tree
and read the diff.

## Deviations from the C API

- Names are spelled out and hyphenated: `imFileOpen` → `im:load`,
  `imProcessReduceBy4` → `im:resize`.
- Enums and bitfields are keywords: `:color-space-rgb`, `:data-type-byte`.
- IM packs a colour space and three configuration bits into one `int`; the
  Lisp API keeps `im:color-space` and `im:color-mode-config` separate.
- C setters become `setf` functions, and out-parameters become multiple values.

## Notes

`libim_jp2` prints a JasPer deprecation banner on startup. It comes from
`jas_init` inside IM's JP2 driver, goes to stderr, and is harmless.

`imProcessBitwiseOp`'s `:xor` is a true exclusive-or in this fork of IM;
upstream's computed NOR, which is available here as `:nor`. Code ported from
stock IM 3.15 changes behaviour silently.

On macOS, `im capture` can enumerate devices from a terminal but connecting to
one requires `NSCameraUsageDescription` in an application bundle. Without it
the process is killed by TCC rather than being allowed to fail.

## License

MIT. See LICENSE.
