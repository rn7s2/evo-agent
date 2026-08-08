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

- *VS Code integrated terminal* (all three platforms) — enable in settings:

  ```json
  "terminal.integrated.enableImages": true,
  "terminal.integrated.gpuAcceleration": "on"
  ```

  Both are required: the image addon only activates on the GPU renderer.
- *kitty* itself works out of the box.
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

## Calibration

The terminal lays images out in **CSS pixels** (xterm.js maps 1 image px to
1 CSS px; a cell is ~9×18 CSS px at default zoom).  Layout — how many rows a
formula reserves, where its baseline lands — is computed from three settings:

```lisp
(evo:set-setting :math-cell-px   18)  ; CSS px per terminal row
(evo:set-setting :math-cell-w-px 9)   ; CSS px per terminal column
(evo:set-setting :math-dpi       220) ; formula size: 110 ≈ prose x-height,
                                      ; 220 = 2x prose (easier to read)
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
| `:math-dpi` | `110` | rasterization size (CSS px) |
| `:math-cell-px` | `18` | terminal row height, CSS px |
| `:math-cell-w-px` | cell-px/2 | terminal column width, CSS px (wrap budget) |
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

- The terminal's image canvas is CSS-resolution: on a retina display a
  formula can never be sharper than a 1× image upscaled — that is the
  terminal's ceiling, not a dpi problem.
- Line spacing around tall formulas is real: a display-size fraction at
  `:math-dpi 220` is ~5 terminal rows and baseline-aligned layout must
  reserve them, exactly as a browser grows line-height around tall inline
  math.  Use a lower `:math-dpi` for tighter paragraphs.
- The live streaming preview (bottom region) shows math as source; the image
  appears when the finished line reaches scrollback.

## Troubleshooting

- **Formulas show as `$…$` source** — no renderer: check `/math status` for
  the toolchain (latex ✓ dvipng ✓) and that math is `on`.
- **Escape garbage in output** — the terminal has no kitty graphics support
  (or VS Code's `enableImages`/`gpuAcceleration` are off).
- **Overlap, or gray dithered boxes** — cell calibration is wrong for your
  font/zoom (images spill over text, then the terminal's tile accounting
  degrades them to placeholders): re-run `tests/math-calibrate.py` and update
  the three geometry settings.
- **Formulas too large / too small** — tune `:math-dpi` (110 matches prose
  x-height at an 18 px cell; scale proportionally with `:math-cell-px`).
