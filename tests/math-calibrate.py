#!/usr/bin/env python3
"""math-calibrate.py — measure the terminal's cell geometry and prove math
placement, from inside the terminal you actually use.

For VS Code, use the evo-vscode extension instead: its webview terminal reports
its device-pixel-ratio and device cell size automatically, so no calibration is
needed.  This script is for OTHER terminals that lay images out in CSS pixels.

Run it AT A SHELL PROMPT in that terminal (not through a subprocess — it needs
the real tty):

    python3 tests/math-calibrate.py

It (1) asks the terminal for its cell size in CSS pixels (CSI 16 t) — the
space xterm.js lays images out in — and prints the matching evo settings;
(2) renders one inline and one display formula with latex+dvipng at the
measured geometry and places them with the exact escape choreography the evo
TUI uses (kitty APC, C=1, sub-cell Y offset, save/restore cursor); (3) reads
the kitty responses so a rejected image is reported as text, not silence.

If prose sits on the formula's baseline with no overlap here, it will in evo.
"""

import base64, os, re, subprocess, sys, termios, tempfile

try:
    TTY = os.open("/dev/tty", os.O_RDWR)
except OSError:
    sys.exit("no tty: run this at a shell prompt in your terminal, "
             "not through a subprocess")
ESC = "\x1b"

def wr(s):
    os.write(TTY, s.encode() if isinstance(s, str) else s)

def read_until(end, timeout=10):
    """Read tty bytes until END char or timeout (deciseconds)."""
    old = termios.tcgetattr(TTY)
    new = termios.tcgetattr(TTY)
    new[3] &= ~(termios.ICANON | termios.ECHO)
    new[6][termios.VMIN] = 0
    new[6][termios.VTIME] = timeout
    termios.tcsetattr(TTY, termios.TCSANOW, new)
    out = ""
    try:
        while True:
            c = os.read(TTY, 1).decode("latin-1", "replace")
            if not c:
                break
            out += c
            if c == end:
                break
    finally:
        termios.tcsetattr(TTY, termios.TCSANOW, old)
    return out

def query(seq, end):
    wr(seq)
    return read_until(end)

def which(name):
    dirs = [os.path.expanduser("~/bin"), "/Library/TeX/texbin",
            "/usr/local/bin", "/opt/homebrew/bin"] + os.environ["PATH"].split(":")
    for d in dirs:
        p = os.path.join(d, name)
        if os.access(p, os.X_OK):
            return p
    return None

def render(latex, display, dpi, fg="white"):
    """(png_bytes, above_px, below_px, width_px) via latex+dvipng."""
    body = ("$\\displaystyle %s$" % latex) if display else ("$%s$" % latex)
    doc = ("\\documentclass[preview,border=1pt]{standalone}\n"
           "\\usepackage{amsmath,amssymb}\\usepackage{xcolor}\n"
           "\\begin{document}\n\\color{%s}\n%s\n\\end{document}\n" % (fg, body))
    with tempfile.TemporaryDirectory() as d:
        tex = os.path.join(d, "f.tex")
        open(tex, "w").write(doc)
        subprocess.run([which("latex"), "-interaction=nonstopmode",
                        "-halt-on-error", "-output-directory=" + d, tex],
                       cwd=d, capture_output=True)
        r = subprocess.run([which("dvipng"), "-q", "--depth", "--height",
                            "-D", str(dpi), "-T", "tight", "-bg", "Transparent",
                            "-o", os.path.join(d, "f.png"),
                            os.path.join(d, "f.dvi")],
                           capture_output=True, text=True)
        above = int(re.search(r"height=(\d+)", r.stdout).group(1))
        below = int(re.search(r"depth=(\d+)", r.stdout).group(1))
        png = open(os.path.join(d, "f.png"), "rb").read()
        width = int.from_bytes(png[16:20], "big")
        height = int.from_bytes(png[20:24], "big")
        # normalize reported baseline to actual PNG height (border etc.)
        tot = above + below
        above = round(above * height / tot)
        below = height - above
        return png, above, below, width

def place(above, below, cellh, frac=0.8):
    """Mirror of the extension's math-place: (total_rows, ascent_rows, y_off)."""
    base_y = round(frac * cellh)
    top = base_y - above
    bottom = base_y + below
    top_row = top // cellh
    bottom_row = max(0, bottom - 1) // cellh
    return (max(1, bottom_row - top_row + 1), max(0, -top_row), top % cellh)

def kitty(png, yoff, img_id):
    """Chunked kitty APC, C=1 (never moves the cursor), explicit id."""
    b64 = base64.standard_b64encode(png).decode()
    first, out, i = True, "", 0
    while i < len(b64) or first:
        chunk, i = b64[i:i + 4096], i + 4096
        more = 1 if i < len(b64) else 0
        if first:
            head = "f=100,a=T,C=1,i=%d%s,m=%d" % (
                img_id, (",Y=%d" % yoff) if yoff else "", more)
            out += "%s_G%s;%s%s\\" % (ESC, head, chunk, ESC)
            first = False
        else:
            out += "%s_Gm=%d;%s%s\\" % (ESC, more, chunk, ESC)
    return out

def kitty_response(img_id):
    r = read_until("\\", timeout=20)
    m = re.search(r"_Gi=%d;([^\x1b]*)" % img_id, r)
    return m.group(1) if m else ("no response" if not r else repr(r))

def cuu(n): return "%s[%dA" % (ESC, n) if n else ""
def cud(n): return "%s[%dB" % (ESC, n) if n else ""
def cuf(n): return "%s[%dC" % (ESC, n) if n else ""

def main():
    for exe in ("latex", "dvipng"):
        if not which(exe):
            print("missing tool:", exe); return 1

    # --- 1. geometry ----------------------------------------------------
    m = re.search(r"\[6;(\d+);(\d+)t", query(ESC + "[16t", "t"))
    if not m:
        print("terminal did not answer CSI 16 t; is this the real tty?"); return 1
    cellh, cellw = int(m.group(1)), int(m.group(2))
    dpi = round(110 * cellh / 18)     # scale prose-match dpi with the cell
    print("cell size:      %d CSS px wide x %d CSS px high" % (cellw, cellh))
    print("evo settings:   (evo:set-setting :math-cell-px   %d)" % cellh)
    print("                (evo:set-setting :math-cell-w-px %d)" % cellw)
    print("                (evo:set-setting :math-dpi       %d)" % dpi)
    print()

    # --- 2. inline placement test --------------------------------------
    png, above, below, width = render(r"e^{i\pi}+1=0", False, dpi)
    total, ascent, yoff = place(above, below, cellh)
    cols = max(1, -(-width // cellw))
    desc = total - 1 - ascent
    print("inline euler:   %dx%d px -> %d rows (%d above baseline), %d cols"
          % (width, above + below, total, ascent, cols))
    h = total
    seq = "\r\n" * (h - 1) + cuu(desc)
    seq += "prose before "
    seq += ESC + "7" + cuu(ascent) + kitty(png, yoff, 41) + ESC + "8" + cuf(cols)
    seq += " prose after: xxx gggg yyy"
    seq += cud(desc) + "\r\n"
    wr(seq)
    r1 = kitty_response(41)
    print("kitty said:     %s" % r1)

    # --- 3. display placement test -------------------------------------
    png, above, below, width = render(
        r"\int_{-\infty}^{\infty} e^{-x^2}\,dx = \sqrt{\pi}", True, dpi)
    total, ascent, yoff = place(above, below, cellh)
    print("display gauss:  %dx%d px -> %d rows" % (width, above + below, total))
    seq = "\r\n" * (total - 1) + cuu(total - 1)
    seq += kitty(png, yoff, 42)
    seq += cud(total - 1) + "\r\n"
    wr(seq)
    r2 = kitty_response(42)
    print("kitty said:     %s" % r2)
    print("this line must sit BELOW the integral, not on it.")
    print()
    print("check: inline euler on the prose baseline, no overlap anywhere,")
    print("both responses OK.  If the cell size differs from your init.lisp,")
    print("copy the three settings above in and restart evo.")
    return 0

if __name__ == "__main__":
    sys.exit(main())
