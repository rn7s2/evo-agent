# LaTeX math rendering

LaTeX math in agent output — `$…$`, `$$…$$`, `\(…\)`, `\[…\]` — renders as
real typeset images inline in scrollback, the way KaTeX/MathJax draw it:
inline formulas sit ON the prose baseline and flow with the text (wrapping by
formula, never through one), display equations get their own block.  With the
feature unavailable, math is simply shown as its LaTeX source — nothing
breaks.

While rendering is active, the extension also registers a system-prompt note
asking the agent to *write* mathematics as LaTeX rather than Unicode
approximations (withdrawn by `/math off`).

## Prerequisites

Two things must exist: a terminal that renders kitty graphics, and a LaTeX
toolchain to rasterize formulas.

**Terminal.**  Images are emitted with the kitty graphics protocol.

- *VS Code* — use the [evo-vscode](https://github.com/rn7s2/evo-vscode)
  extension's **Evo: Open in CLI** command.  It runs evo in its own webview
  terminal that renders images at **device resolution**, so formulas are crisp
  on Retina/HiDPI.  (VS Code's *built-in* terminal also supports kitty graphics
  via `terminal.integrated.enableImages` + `gpuAcceleration`, but it draws into
  a CSS-resolution canvas, so on a HiDPI display every formula is upscaled and
  blurry — no DPI or supersampling can beat that ceiling.  Prefer the
  extension.)
- *kitty*, *Ghostty*, *WezTerm* — device-resolution kitty graphics out of the
  box; formulas are crisp.
- Terminals with no kitty-graphics support show escape garbage — use
  `/math off` (or leave the toolchain uninstalled; the feature then never
  activates).

**LaTeX toolchain.**  `latex` and `dvipng`, with the `standalone`, `preview`,
`amsmath`, `amssymb`, and `xcolor` packages.

- macOS: full [MacTeX](https://tug.org/mactex/) has everything; for the small
  install, `brew install --cask basictex` then
  `sudo tlmgr install dvipng standalone preview`.
- Debian/Ubuntu: `apt install texlive-latex-extra dvipng`.
- Fedora: `dnf install texlive-standalone texlive-dvipng`.

Binaries are searched on `PATH` plus the usual TeX locations (`~/bin`,
`/Library/TeX/texbin`, `/usr/local/bin`, `/opt/homebrew/bin`,
`/usr/local/texlive/bin`) — a GUI-launched evo without a login shell's `PATH`
still finds them.  `/math status` shows what was found.

**CJK / Unicode fallback (optional).**  The classic `latex` engine is 8-bit and
cannot set CJK or other non-Latin Unicode, so a formula like
`\prod_{p\ \text{素}} ...` fails it and would otherwise fall back to raw source.
When that happens and `xelatex` (with the `xeCJK` package and a CJK font — e.g.
Fandol, shipped with TeX Live/MiKTeX) plus a PDF rasterizer (`pdftocairo`, or
ImageMagick+Ghostscript, or `pdftoppm`) are present, evo retries the formula
through XeLaTeX and renders it as an image just like any other.  ASCII math never
leaves the fast `latex`+`dvipng` path.  `/math status` shows `xelatex ✓ (CJK)`
when the fallback is available.

- macOS: MacTeX/MiKTeX include `xelatex` + `xeCJK`; `brew install poppler` gives
  `pdftocairo`.
- Debian/Ubuntu: `apt install texlive-xetex texlive-lang-cjk poppler-utils`.

## Calibration

> **In the [evo-vscode](https://github.com/rn7s2/evo-vscode) webview terminal
> this is automatic.**  That terminal reports its device-pixel-ratio and exact
> **device** cell size through the environment (`EVO_WEBVIEW_DPR`,
> `EVO_TERM_CELL_W_PX` / `EVO_TERM_CELL_H_PX`); the renderer then sizes formulas
> in device pixels and rasterizes at `:math-dpi × dpr`, so they are crisp at the
> right size with nothing to calibrate.  Set the geometry below **only** for
> other terminals.

Most terminals lay images out in **CSS pixels** (xterm.js maps 1 image px to
1 CSS px; a cell is ~9×18 CSS px at default zoom).  Layout — how many rows a
formula reserves, where its baseline lands — is computed from three settings:

```lisp
(evo:set-setting :math-cell-px   18)  ; CSS px per terminal row
(evo:set-setting :math-cell-w-px 9)   ; CSS px per terminal column
(evo:set-setting :math-dpi       110) ; formula size: 110 ≈ prose x-height,
                                      ; 220 = 2× prose (easier to read)
```

Run `python3 tests/math-calibrate.py` **at a shell prompt in the terminal you
use** after changing font size or zoom: it queries the terminal for its exact
cell geometry (`CSI 16 t`), prints the settings to paste into `init.lisp`,
and paints an inline + a display test with the exact escape choreography the
TUI uses, reading back the terminal's accept/reject responses.

## Settings

| setting | default | meaning |
|---|---|---|
| `:math` | `t` | master switch |
| `:math-dpi` | `110` | rasterization size (CSS px); the webview renders at `× dpr` |
| `:math-cell-px` | `18` | terminal row height, CSS px; the webview overrides with its device cell |
| `:math-cell-w-px` | cell-px/2 | terminal column width, CSS px (wrap budget); webview-overridden |
| `:math-baseline-frac` | `0.8` | where the text baseline sits within a row |
| `:math-snap-px` | `2` | nudge ≤ this many px off true baseline when it saves a whole terminal row |
| `:math-x-advance` | `:terminal` | inline stepping: `:terminal` lets the terminal advance the cursor past the image by its own exact column count; `:manual` pins with `C=1` and steps by the estimate |
| `:math-pixel-align` | `t` | sub-cell `Y=` offset for a pixel-exact baseline |
| `:math-foreground` | `nil` | xcolor glyph colour; `nil` follows `:theme` |
| `:math-border` | `"1pt"` | whitespace around each formula |
| `:math-max-bytes` | `786432` | largest PNG to emit; bigger falls back to source |
| `:math-inline-mode` | `:aligned` | `:aligned` baseline layout; `:break` one image per line (safe fallback); `:raw` no correction |

`/math status | on | off | clear-cache` at runtime.  The glyph colour follows
`/theme dark | light`.  Settings are read per render, so changes apply to the
next formula; the disk cache (`EVO_HOME/cache/math`) keys on content and
settings, so stale entries are simply unused (`/math clear-cache` tidies).

## Limitations

- **Sharpness depends on the terminal.**  Native kitty terminals (kitty,
  Ghostty, WezTerm) and the [evo-vscode](https://github.com/rn7s2/evo-vscode)
  webview draw images at **device resolution** — formulas are crisp on retina.
  Most other terminals — including VS Code's *built-in* terminal — draw into a
  CSS-resolution canvas, so on a retina display a formula can never be sharper
  than a 1× image upscaled; that is the terminal's ceiling, not a dpi problem.
- Line spacing around tall formulas is real: a display-size fraction at
  `:math-dpi 220` is ~5 terminal rows and baseline-aligned layout must
  reserve them, exactly as a browser grows line-height around tall inline
  math.  Use a lower `:math-dpi` for tighter paragraphs.
- The live streaming preview (bottom region) shows math as source; the image
  appears when the finished line reaches scrollback.

## Troubleshooting

- **Formulas show as `$…$` source** — no renderer: check `/math status` for
  the toolchain (latex ✓ dvipng ✓) and that math is `on`.
- **Only CJK/Unicode formulas show as source** — the classic engine cannot set
  those; install the fallback (`/math status` should read `xelatex ✓ (CJK)`) —
  see the CJK / Unicode fallback under Prerequisites — then `/math clear-cache`.
- **Escape garbage in output** — the terminal has no kitty graphics support.
  In VS Code, run evo through the [evo-vscode](https://github.com/rn7s2/evo-vscode)
  extension (its webview terminal renders images at device resolution); the
  built-in terminal needs `terminal.integrated.enableImages` + `gpuAcceleration`
  on and is only CSS-resolution.
- **Overlap, or gray dithered boxes** — cell calibration is wrong for your
  font/zoom (images spill over text, then the terminal's tile accounting
  degrades them to placeholders).  Other terminals only: re-run
  `tests/math-calibrate.py` and update the three geometry settings (the
  evo-vscode webview measures its cell size automatically, so this cannot
  happen there).
- **Formulas too large / too small** — tune `:math-dpi` (110 matches prose
  x-height at an 18 px cell; scale proportionally with `:math-cell-px`).  In the
  evo-vscode webview only `:math-dpi` matters — the cell size is auto-detected.
