# Debugging evo on Windows

This is a field manual, not a design document. Windows was brought up from a
Unix machine with no access to a Windows one, so everything here was paid for
in round trips: a question, a file mailed across, an answer, a fix. It is
written down so the next person — or the next evo, running on Windows itself —
starts from the answers instead of buying them again.

Two things it gives you: **what is already settled**, with the measurements
that settled it, and **the instruments**, which are checked in and meant to be
extended rather than reinvented.

## What is already settled

Each of these cost at least one round trip. None of them is guesswork; the
measurement is quoted so you can tell when a future SBCL has changed its mind.

### A file descriptor is an OS handle

`SB-SYS:MAKE-FD-STREAM` on Windows wants a handle, not a C descriptor. The
TUI's first write died with

```
Couldn't write to #<SB-SYS:FD-STREAM for "descriptor 1">: The handle is invalid.
```

because it asked for descriptor 1. Measured in a console: `*stdout*`'s fd is
116 and `GetStdHandle(STD_OUTPUT_HANDLE)` is 116 — the same number. In the
supervised child the same stream is 12: **handles are per process**, so
nothing may be hardcoded, not even after you learn what the console's are.

`EVO.PORT:STD-DESCRIPTOR` takes the number out of the standard stream SBCL
built for itself at startup. That is right on every platform and stays right
when stdout is a pipe or a file.

### The console is written UCS-2LE, and forcing UTF-8 kills the process

A console handle goes through the wide console API. A stream forced to
`:external-format :utf-8` hands it UTF-8 bytes that it reads as UTF-16 code
units: `[probe marker via the fd of SBCL's own *stdout*]` came out as

```
灛潲敢洠牡敫⁲楶⁡桴⁥摦漠⁦䉓䱃猧漠湷⨠瑳潤瑵崪
```

— each pair of ASCII bytes rendered as one CJK glyph. Worse than cosmetic:
the write then **killed the process outright**, three runs, same place, no
Lisp condition to catch.

`EVO.PORT:STD-EXTERNAL-FORMAT` copies what SBCL chose for its own console
stream (`(:UCS-2LE :REPLACEMENT #\? ...)`), and keeps a deliberate `:utf-8` on
Unix, where the locale may say C but the box drawing still has to render.
Verified: ASCII and `── ❯ ○` both print correctly at console codepage 936.

### Console input reaches you through neither bytes nor characters

Both obvious paths lose something:

- **Bytes off the handle** return UTF-16LE. Typing `abc` arrives as
  `97 0 98 0 99 0`; the up arrow as `27 0 91 0 65 0`. Every byte the key
  parser understands is followed by a NUL.
- **Characters from SBCL's console stream** are decoded correctly, but the
  layer doing it also translates newlines, *below* the external format —
  `:newline :lf` does not disable it. In VT input mode Enter sends a bare CR,
  which was then held back waiting for an LF that never came. The symptom was
  precise and misleading: Enter did nothing at all, while ctrl+Enter (which
  sends LF) submitted, because its LF completed the pair. The input trace
  showed **no byte whatsoever** for the Enter keypress.

So `EVO.PORT:DRAIN-CONSOLE-INPUT` reads key events with `ReadConsoleInputW`
and encodes them itself: the character untranslated when the key carries one,
`ESC` before it when alt is held, surrogate pairs joined across reads, and the
CSI spelling the parser already reads for keys that carry no character
(arrows, home, end, delete, page up/down).
`GetNumberOfConsoleInputEvents` keeps it non-blocking, so the poll loop is
unchanged. A redirected stdin is not a console and keeps the stream path.

### CR-LF source breaks the build, in one specific way

Git for Windows checks out with `core.autocrlf=true`. CR is whitespace to the
reader, so a CR-LF tree compiles fine — except inside a string, where the
`~<newline>` FORMAT continuation reads the character after the tilde and finds
CR: *"error in FORMAT: Unknown directive (character: Return)"*, at compile
time, in every file that uses it.

The codebase no longer contains that construct (`EVO.UTIL:CAT` concatenates
literals instead, and `TEST-LINE-ENDINGS` fails the suite if one reappears),
and text that crosses a boundary is normalized (`EVO.UTIL:NORMALIZE-NEWLINES`).
`.gitattributes` still pins the tree to LF, because the `.sh` and `.exp`
suites have no such shield — a `#!/bin/sh` with a CR is a bad interpreter.

`tests/windows-probe.lisp` is deliberately kept CR-LF as a standing check.

### dexador ignores `:proxy` on Windows

dexador speaks WinHTTP there (`dex:*default-backend*` is `:winhttp`; the
usocket backend is not even compiled, because it would need OpenSSL). That
backend has the argument in a `declare ignore` and a `;; TODO: proxy support`
beside it — its own issue #66 closed with *"Proxy support: winhttp doesn't
support it"*. Worse than ignoring it: the library it calls passes
`WINHTTP_ACCESS_TYPE_NO_PROXY` when given no proxy, so every request is told
explicitly to go direct and no system setting (`netsh winhttp`, IE,
`http_proxy`) can override it. The symptom is quiet: reachable hosts work,
blocked ones time out with `ERROR 12002`, and the proxy log stays empty.

The library grew the argument since: `WINHTTP:HTTP-OPEN` takes a proxy and
then passes `WINHTTP_ACCESS_TYPE_NAMED_PROXY`. `EVO.UTIL:ENSURE-WINHTTP-PROXY`
wraps that one function and hands it the proxy evo already resolved for the
request; `EVO.UTIL:WITH-PROXY` is what every HTTP call in evo (and in an
extension) goes through. Upstream is implementing this properly in #202 —
when it lands, the wrapper goes.

### A running evo.exe cannot be rebuilt over

Windows locks a running image. `save-lisp-and-die` then reports
`build\evo.exe: Permission denied` and the build dies. **Close the running evo
first** — both processes, the supervisor and its child. This is deliberately
not worked around in `build.lisp`.

### The supervisor used to bury the error it was reporting

`--resume` is a guess the supervisor makes on the user's behalf, and a child
that dies before the model has answered has journalled nothing. Passing
`--resume` then turned one real failure into five copies of a misleading one
("No sessions to resume") while the real error scrolled away. `RESTART-ARGV`
now checks the guess, the restart line names the arguments actually used, and
an explicit `--resume` with nothing to resume exits 64 — the one code the
supervisor never restarts.

## The instruments

### `tests/windows-probe.lisp` — ask the machine

Self-contained: plain SBCL, no evo, no quicklisp, no `sb-posix` (Windows SBCL
does not have it). Every probe is wrapped so one failure cannot hide the rest,
and every line is flushed as it is written, so a hard crash still leaves
everything learned up to that point on disk — which is exactly what happened
when the forced-UTF-8 write killed the process.

```powershell
sbcl --script .\tests\windows-probe.lisp
```

It appends to `%USERPROFILE%\evo-windows-probe.txt`. It can also be dropped in
`%USERPROFILE%\.evo\extensions\000-probe.lisp` to run inside the real binary,
in the supervised child — worth doing, since that is where handles differ.

Rules that made it work, if you write another round:

- **Report to a file, not to the console.** The console is the thing under
  test; its output may be mojibake or absent.
- **One question per probe, with both candidates measured side by side.** The
  useful line was never "it failed" but "1 fails and 116 works".
- **Print the raw numbers** (decimal and hex, and the printable spelling of
  byte lists). Conclusions can be drawn later; numbers cannot be recovered.
- **Order probes so the risky one is last**, and flush before it.

### `EVO_INPUT_TRACE` — watch the keys arrive

```powershell
$env:EVO_INPUT_TRACE="$env:USERPROFILE\evo-input-trace.txt"
.\build\evo.exe
```

Three lines per tick, and they separate the three suspects in one run:

```
 4745 bytes  (13)                 <- what the keyboard actually delivered
 4747 keys   (:ENTER)             <- what the parser made of it
 4748 events (:ENTER)             <- what survived paste coalescing
```

| Symptom | Culprit |
| --- | --- |
| no `bytes` line for a keypress | below us: the console or SBCL's layer |
| `bytes` but no `keys` | the byte parser in `src/tui/input.lisp` |
| `keys` but no `events` | paste-burst coalescing (`COALESCE-PASTE-BURST`) holds a trailing Enter until the next batch |

Off unless the variable is set. It is how Enter was diagnosed from another
continent, and it should be the first thing reached for, before any theory.

### `tests/windows-input-live.lisp` — inject real keys, assert real bytes

Opens `CONIN$` (the console input buffer, reachable even when stdin is a
redirected pipe), writes genuine `INPUT_RECORD`s with `WriteConsoleInputW`,
and asserts the exact UTF-8 bytes `EVO.PORT:DRAIN-CONSOLE-INPUT` produces. It
drives the real code path the TUI uses, so it is proof, not a model.

```powershell
sbcl --non-interactive --load tests\windows-input-live.lisp   # or: .\make.ps1 console-test
```

One caveat learned the hard way: `WriteConsoleInput` normalizes an injected
`wRepeatCount` back to 1, so a held-key repeat cannot be injected this way
(the `(max 1 repeat)` loop is correct by inspection).

### `tests/windows-console-live.lisp` — size, raw mode, and the glyphs

Opens `CONOUT$`/`CONIN$` and checks `console-size`, the `SetConsoleMode`
raw-mode round trip, and — the interesting one — writes the exact octets evo's
stdout stream would emit and reads the glyphs back with
`ReadConsoleOutputCharacterW`, so box drawing and CJK are verified as *stored
code points*, not eyeballed. (`make-fd-stream` over a non-std console handle
hangs, so the test writes bytes with `WriteFile` — the same call the fd-stream
ultimately makes.)

### The suites

`.\make.ps1 test` runs the unit suite (the whole safety net on Windows — the
`.sh`/`.exp` suites are Unix-only). `.\make.ps1 console-test` runs the two live
console tests above, which need a real console and so cannot run in CI.
`.\make.ps1 build` builds.

## Verified on real hardware

Each line below was observed working on Windows itself, by the instrument
named. When one regresses, run that instrument and take a trace before
theorising.

- **Arrows, Home, End, Delete, Insert, PgUp, PgDn** — `windows-input-live.lisp`,
  25/25: every synthesised CSI spelling in the virtual-key table matches, plus
  letters, Enter-as-CR, tab, backspace, esc, ctrl+c, alt+key (ESC-prefixed),
  CJK, accented Latin, and an emoji surrogate pair joined across records.
- **CJK and box drawing** — `windows-console-live.lisp`: U+2500/U+276F/U+25CB
  and é round-trip exactly through the console at codepage 65001, and the
  double-width CJK ideographs U+4E2D/U+6587 are stored, not mojibake'd.
- **Resize** — the engine (`terminal-size` / `GetConsoleScreenBufferInfo`)
  returns the real window dimensions in `windows-console-live.lisp`; the
  per-tick poll compares them and sets `*resized*`.
- **Raw mode** — `SetConsoleMode` enables VT input + processing and clears
  line/echo, and restore puts the modes back exactly (same instrument).
- **The supervisor path end to end** — crash → restart → quarantine
  (`--no-userspace`) → the exit-code protocol (1 restarts, 64 stops),
  observed with a real `evo.exe` child that logs each boot then exits 1.
  When testing this, capture the supervisor's stderr with `Start-Job` + `cmd`
  redirection: running it directly under another agent's shell tool truncates
  the child tree and makes it look like it only booted once.
- **The bash tool** runs through PowerShell (a temp script; see below).
  Latency note: pwsh startup here is ~1.5–7 s per call — that is Defender/AMSI
  scanning plus pwsh's own start cost, not evo (a fresh `-EncodedCommand` is
  just as slow, and `cmd` is ~0.07 s); the abort test widens its Windows
  timing margin accordingly.

## Still worth a look on real hardware

- **Paste bursts.** The byte→key→event path is proven, but injecting a real
  Windows Terminal paste (with or without the bracketed-paste wrapper) and
  confirming `[pasted N lines]` was not automated. The burst heuristic
  (`*paste-burst-min-chars*`) is what catches an unwrapped paste.
- **Held-key auto-repeat** in a live TUI (see the `WriteConsoleInput` caveat
  above — the count cannot be injected).
- **The modifyOtherKeys / kitty spellings** the parser asks for at startup, as
  Windows Terminal actually emits them.

## When evo runs on Windows itself

The point of all this is to stop mailing files across. Once evo runs there, it
can hold both ends:

- It can run the probe and read the report with its own tools; both are plain
  files.
- It can set `EVO_INPUT_TRACE` before launching a second evo and read the
  trace — but note the variable must be in the environment of the *child*,
  which the supervisor inherits, so setting it in the shell before launching
  is enough.
- It **cannot rebuild itself while running**: the image is locked. Iterate
  with `load-extension` and `/eval` in the live image, which is what
  self-extension is for, and rebuild only when the session can be closed.
- The `bash` tool is PowerShell there. Commands that assume `/bin/sh` will
  fail; the tool description says so, and the port layer writes a temp script
  rather than an argv, because the Windows command line is one string that
  each interpreter re-splits by its own rules.

The method that worked is worth keeping too, and it is short: **measure before
fixing, and ask the implementation instead of asserting.** Every fix in the
list above has the same shape — `STD-DESCRIPTOR`, `STD-EXTERNAL-FORMAT`,
`DRAIN-CONSOLE-INPUT` — take the answer from the system that knows it, rather
than encoding a belief about what a platform means by "descriptor", "UTF-8",
or "a key". Three of the four bugs came from a literal that was true on Unix.
