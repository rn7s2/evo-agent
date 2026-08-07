;;;; media.lisp — images as first-class input: the read path.
;;;;
;;;; The message model has always carried an :image content block; what was
;;;; missing was everything that produces one.  This module is that half:
;;;; bytes in (a file, the system clipboard), a validated :image block out.
;;;;
;;;; Three rules shape it:
;;;;
;;;;  - Errors are data.  Every entry point returns (values BLOCK REASON):
;;;;    the callers are a keystroke handler and a slash command, and a
;;;;    mistyped path must never unwind a TUI thread.
;;;;  - Images travel by value, base64 in the block, and therefore into the
;;;;    journal.  A session stays replayable with no side files to lose —
;;;;    the same reason the transcript itself is data (D2).  The price is
;;;;    session-file size, which *MAX-IMAGE-BYTES* bounds.
;;;;  - The clipboard is a platform tool, not a syscall: each reader shells
;;;;    out to the one program that owns the pasteboard on its platform
;;;;    (osascript, wl-paste, xclip) and writes a file.  *CLIPBOARD-READERS*
;;;;    is an ordered, rebindable list, so a platform evo does not ship
;;;;    support for is an extension away rather than a kernel patch.

(in-package :evo.media)

(defparameter *max-image-bytes* (* 3 1024 1024)
  "Largest image evo will send, in raw bytes.  Base64 inflates by 4/3, so
this keeps a request under the 5 MB per-image ceiling Anthropic enforces —
and keeps a session file from doubling in size per screenshot.  Anything
bigger is downscaled first (see SHRINK-IMAGE-FILE); if that is impossible,
the attach fails loudly rather than the request failing at the provider.")

(defparameter *max-image-dimension* 1568
  "Long-edge pixel target when downscaling an oversized image.  Both major
vision stacks resize larger images down to roughly this before tokenizing,
so shrinking on our side costs no fidelity and saves upload and tokens.")

;;; Media types.
;;;
;;; Sniffed from magic bytes, never from the file name: the extension is a
;;; user-supplied claim, and sending a mislabelled media_type is a provider
;;; error the user cannot read.  These four are the intersection of what
;;; Anthropic Messages and OpenAI Responses both accept.

(defparameter +image-magic+
  '(("image/png"  #(137 80 78 71 13 10 26 10))
    ("image/jpeg" #(255 216 255))
    ("image/gif"  #(71 73 70 56)))      ; GIF8
  "Alist of media type -> leading magic bytes.")

(defun media-type-extension (media-type)
  (cond ((equal media-type "image/png") "png")
        ((equal media-type "image/jpeg") "jpg")
        ((equal media-type "image/gif") "gif")
        ((equal media-type "image/webp") "webp")
        (t "img")))

(defun prefix-match-p (magic octets)
  (and (>= (length octets) (length magic))
       (every (lambda (a b) (= a b)) magic (subseq octets 0 (length magic)))))

(defun sniff-media-type (octets)
  "Media type of OCTETS from its magic bytes, or NIL if it is not an image
evo can send."
  (or (loop for (type magic) in +image-magic+
            when (prefix-match-p magic octets) return type)
      ;; RIFF....WEBP — the magic is split by a 4-byte length field.
      (and (>= (length octets) 12)
           (prefix-match-p #(82 73 70 70) octets)                 ; RIFF
           (prefix-match-p #(87 69 66 80) (subseq octets 8 12))   ; WEBP
           "image/webp")))

(defun file-media-type (path)
  "Media type of the file at PATH sniffed from its first bytes, or NIL."
  (ignore-errors
   (with-open-file (in path :direction :input :element-type '(unsigned-byte 8)
                            :if-does-not-exist nil)
     (when in
       (let* ((head (make-array 16 :element-type '(unsigned-byte 8)))
              (n (read-sequence head in)))
         (sniff-media-type (subseq head 0 n)))))))

(defun image-file-p (path)
  (and (probe-file path) (file-media-type path) t))

;;; Image content blocks.

(defun make-image-block (&key data media-type name bytes source)
  "An :image content block: the unified form every provider adapter encodes."
  (list :type :image
        :media-type media-type
        :data data
        :name (or name "image")
        :bytes (or bytes 0)
        :source (or source "file")))

(defun image-block-p (block)
  (eq (pget block :type) :image))

(defun format-bytes (n)
  (cond ((null n) "?")
        ((< n 1024) (format nil "~d B" n))
        ((< n (* 1024 1024)) (format nil "~,1f KB" (/ n 1024.0)))
        (t (format nil "~,1f MB" (/ n (* 1024.0 1024.0))))))

(defun image-summary (block)
  "One-line human description of an image block: name, type, size."
  (format nil "~a · ~a · ~a"
          (pget block :name "image")
          (media-type-extension (pget block :media-type))
          (format-bytes (pget block :bytes))))

;;; Child processes: the clipboard and the downscalers are external programs.

(defun scratch-file (stem extension)
  "A fresh path in the system temp directory.  Callers delete it themselves;
image bytes live in the journal, never on the side."
  (merge-pathnames (format nil "evo-~a-~a.~a" stem (gen-id 8) extension)
                   (uiop:temporary-directory)))

(defun run-child (program args &key output (timeout 20))
  "Run PROGRAM (looked up on PATH) with ARGS, waiting up to TIMEOUT seconds.
OUTPUT is a pathname stdout is redirected to, or NIL to discard it.  Returns
the exit code, or NIL when the program is not installed, died on a signal,
or timed out (in which case it is killed).  stderr is always discarded — a
missing clipboard tool is a normal condition here, not a failure to report."
  (let ((exe (evo.port:program-in-path program)))
    (when exe
      (let ((process (evo.port:launch-child (namestring exe) args
                                            :input nil
                                            :output (or output nil)
                                            :error-output nil)))
        (handler-case
            (evo.port:call-with-timeout
             timeout
             (lambda ()
               (multiple-value-bind (status code) (evo.port:process-wait process)
                 (and (eq status :exited) code))))
          (error ()
            (ignore-errors (evo.port:process-kill process))
            nil))))))

(defun run-child-output (program args &key (timeout 20))
  "Run PROGRAM and return its stdout as a trimmed string, or NIL on failure."
  (let ((tmp (scratch-file "out" "txt")))
    (unwind-protect
         (when (eql 0 (run-child program args :output tmp :timeout timeout))
           (ignore-errors (string-trim '(#\Space #\Newline #\Return #\Tab)
                                       (read-file-string tmp))))
      (ignore-errors (delete-file tmp)))))

;;; Downscaling: only ever attempted when an image is over the size cap.

(defun file-size (path)
  (or (ignore-errors
       (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in)))
      0))

(defvar *downscalers*
  ;; sips ships with macOS; magick/convert is ImageMagick anywhere else.  The
  ;; trailing ">" in an ImageMagick geometry means "shrink, never enlarge".
  (list
   ;; Pass 1: same format, fewer pixels.
   (list :keep-format "sips"
         (lambda (in out dim) (list "-Z" dim in "--out" out)))
   (list :keep-format "magick"
         (lambda (in out dim) (list in "-resize" (format nil "~ax~a>" dim dim) out)))
   (list :keep-format "convert"
         (lambda (in out dim) (list in "-resize" (format nil "~ax~a>" dim dim) out)))
   ;; Pass 2: a photo or a noisy screenshot that is still too big losslessly.
   ;; JPEG makes detail cost bytes instead of being free.
   (list :jpeg "sips"
         (lambda (in out dim)
           (list "-Z" dim "-s" "format" "jpeg" "-s" "formatOptions" "80" in "--out" out)))
   (list :jpeg "magick"
         (lambda (in out dim)
           (list in "-resize" (format nil "~ax~a>" dim dim) "-quality" "80" out)))
   (list :jpeg "convert"
         (lambda (in out dim)
           (list in "-resize" (format nil "~ax~a>" dim dim) "-quality" "80" out))))
  "Ordered (PASS PROGRAM ARGS-FN) downscalers tried when an image is over the
size cap.  PASS is :keep-format or :jpeg (which decides the output
extension); ARGS-FN builds the argv from in-path, out-path and the pixel
target.  A test binds this to NIL to exercise the no-downscaler path.")

(defun shrink-image-file (path media-type)
  "Downscale PATH to fit *MAX-IMAGE-DIMENSION* on its long edge and, if that
is not enough, to JPEG as well.  Returns the path of a new temp file that is
under the size cap, or NIL when no downscaler is installed (or none of them
got the image small enough)."
  (let ((dim (princ-to-string *max-image-dimension*)))
    (loop for (pass program args-fn) in *downscalers*
          for out = (scratch-file "shrunk" (if (eq pass :jpeg)
                                               "jpg"
                                               (media-type-extension media-type)))
          do (if (and (eql 0 (run-child program
                                        (funcall args-fn (namestring path)
                                                 (namestring out) dim)
                                        :timeout 60))
                      (image-file-p out)
                      (<= (file-size out) *max-image-bytes*))
                 (return out)
                 (ignore-errors (delete-file out))))))

;;; Attaching: file (or clipboard temp file) -> :image block.

(defun attach-image-file (path &key name (source "file"))
  "Read PATH into an :image content block.  Returns (values BLOCK NIL) on
success and (values NIL REASON) on failure — the caller is a key handler,
so a bad path is a message, not a stack unwind."
  (let ((file (ignore-errors
               (probe-file (if (stringp path) (or (token->path path) path) path)))))
    (cond
      ((null file)
       (values nil (format nil "no such file: ~a" path)))
      ((uiop:directory-pathname-p file)
       (values nil (format nil "~a is a directory" path)))
      (t
       (let ((media-type (file-media-type file)))
         (cond
           ((null media-type)
            (values nil (format nil "~a is not a png, jpeg, gif or webp image"
                                (file-namestring file))))
           (t
            (let* ((size (with-open-file (in file :element-type '(unsigned-byte 8))
                           (file-length in)))
                   (shrunk (when (> size *max-image-bytes*)
                             (shrink-image-file file media-type)))
                   (final (or shrunk file)))
              (unwind-protect
                   (let ((final-size (with-open-file (in final :element-type '(unsigned-byte 8))
                                       (file-length in))))
                     (if (> final-size *max-image-bytes*)
                         (values nil
                                 (format nil "~a is ~a — over the ~a limit~a"
                                         (file-namestring file) (format-bytes size)
                                         (format-bytes *max-image-bytes*)
                                         (if shrunk
                                             " even after downscaling; resize it first"
                                             "; install sips or imagemagick to auto-downscale, or resize it first")))
                         (let ((octets (read-file-octets final)))
                           (values (make-image-block
                                    :data (octets->base64 octets)
                                    :media-type (or (sniff-media-type octets) media-type)
                                    :name (or name (file-namestring file))
                                    :bytes final-size
                                    :source source)
                                   nil))))
                (when shrunk (ignore-errors (delete-file shrunk))))))))))))

;;; The clipboard.
;;;
;;; A reader returns the pathname of an image file — one it wrote into the
;;; scratch directory it was handed, or one the clipboard merely pointed at
;;; (a file copied in a file manager).  NIL means "not this platform, or no
;;; image on the clipboard".  Only files inside the scratch directory are
;;; deleted afterwards, so pointing at a user's file never destroys it.

(defparameter *macos-clipboard-script*
  "set outPath to \"~a\"
set fmt to \"\"
try
	set theData to the clipboard as «class PNGf»
	set fmt to \"png\"
end try
if fmt is \"\" then
	try
		set theData to the clipboard as TIFF picture
		set fmt to \"tiff\"
	end try
end if
if fmt is \"\" then
	try
		return \"path:\" & (POSIX path of (the clipboard as «class furl»))
	end try
	return \"none\"
end if
try
	set fh to open for access (POSIX file outPath) with write permission
	set eof fh to 0
	write theData to fh
	close access fh
on error errMsg
	try
		close access fh
	end try
	return \"none\"
end try
return fmt"
  "AppleScript that dumps the pasteboard's image to a file.  PNG first (what
a screenshot and most copy-image gestures put there), TIFF second (older
apps), and finally a file reference — copying an image file in Finder puts
only a «class furl» on the pasteboard, and that gesture should work too.")

(defun macos-clipboard-image (dir)
  (let* ((out (merge-pathnames (format nil "clip-~a.png" (gen-id 8)) dir))
         (result (run-child-output
                  "osascript"
                  (list "-e" (format nil *macos-clipboard-script* (namestring out))))))
    (cond
      ((null result) nil)
      ((string= result "png") (and (image-file-p out) out))
      ;; TIFF is not a media type any provider takes: convert or give up.
      ((string= result "tiff")
       (let ((tiff (merge-pathnames (format nil "clip-~a.tiff" (gen-id 8)) dir)))
         (ignore-errors (rename-file out tiff))
         (when (eql 0 (run-child "sips" (list "-s" "format" "png" (namestring tiff)
                                              "--out" (namestring out))))
           (and (image-file-p out) out))))
      ((string-prefix-p "path:" result)
       (let ((path (probe-file (subseq result 5))))
         (and path (image-file-p path) path)))
      (t nil))))

(defun stdout-clipboard-bytes (program args-for-type dir)
  "Try each image media type against a clipboard tool that writes the data
to stdout (wl-paste, xclip).  The tool is asked for one MIME type at a time
because that is the only question these tools answer."
  (loop for (type . ext) in '(("image/png" . "png") ("image/jpeg" . "jpg")
                              ("image/webp" . "webp") ("image/gif" . "gif"))
        for out = (merge-pathnames (format nil "clip-~a.~a" (gen-id 8) ext) dir)
        do (let ((code (run-child program (funcall args-for-type type) :output out)))
             (if (and (eql 0 code) (image-file-p out))
                 (return out)
                 (ignore-errors (delete-file out))))))

(defun stdout-clipboard-uri-list (program args-for-type dir)
  "The file-manager copy: Nautilus, Dolphin and Thunar put no pixels on the
clipboard, only `text/uri-list` naming the files.  This is the X11/Wayland
twin of Finder's «class furl», and without it the same gesture that works on
macOS silently finds nothing on Linux.  Returns the first URI that is an
image the user owns — never a copy, so the file is not deleted afterwards."
  (let ((out (merge-pathnames (format nil "uris-~a.txt" (gen-id 8)) dir)))
    (unwind-protect
         (when (eql 0 (run-child program (funcall args-for-type "text/uri-list")
                                 :output out))
           (loop for line in (uiop:split-string (or (ignore-errors (read-file-string out)) "")
                                                :separator '(#\Newline #\Return))
                 for token = (string-trim '(#\Space #\Tab) line)
                 ;; Skip blanks, RFC 2483 comments, and the verb line a
                 ;; file manager's own cut/copy flavour starts with.
                 unless (or (zerop (length token))
                            (string-prefix-p "#" token)
                            (member token '("copy" "cut") :test #'string-equal))
                   do (let* ((path (token->path token))
                             (file (and path (ignore-errors (probe-file path)))))
                        (when (and file (image-file-p file)) (return file)))))
      (ignore-errors (delete-file out)))))

(defun stdout-clipboard-reader (program args-for-type dir)
  "Pixels first, then the files the clipboard merely points at."
  (or (stdout-clipboard-bytes program args-for-type dir)
      (stdout-clipboard-uri-list program args-for-type dir)))

(defun wayland-clipboard-image (dir)
  (stdout-clipboard-reader
   "wl-paste" (lambda (type) (list "--no-newline" "--type" type)) dir))

(defun x11-clipboard-image (dir)
  (stdout-clipboard-reader
   "xclip" (lambda (type) (list "-selection" "clipboard" "-t" type "-o")) dir))

;;; WSL: the clipboard is on the other side of the kernel.
;;;
;;; A WSL session is Linux — evo runs there unchanged — but the clipboard
;;; the user copies into belongs to Windows, and no Linux tool can see it
;;; unless WSLg is bridging one.  Without this reader the most common
;;; Windows workflow of all (Win+Shift+S, screenshot to clipboard) hits a
;;; wall in a session that is otherwise perfectly ordinary, so evo asks
;;; Windows itself: PowerShell is reachable from WSL by name, and interop
;;; gives it back a Windows path that maps onto the Linux filesystem.

(defparameter *wsl-powershell-programs*
  '("powershell.exe" "pwsh.exe" "pwsh" "powershell")
  "PowerShell spellings tried, in order, to reach the Windows clipboard.
Rebindable so a test can stand a stub in for the real thing.")

(defparameter *wsl-mount-root* "/mnt"
  "Where WSL mounts the Windows drives.  Configurable in wsl.conf
(automount.root), so it is a parameter here rather than a constant — and
that makes the Windows-to-Linux path mapping testable off WSL.")

(defvar *wsl-session* :unknown
  "T / NIL once WSL-P has looked, :UNKNOWN before.  Tests bind it.")

(defun wsl-p ()
  "Is this a WSL session?  /proc/version names the Microsoft kernel; the
env vars catch custom kernels where it does not."
  (when (eq *wsl-session* :unknown)
    (setf *wsl-session*
          (let ((version (or (ignore-errors (read-file-string "/proc/version")) "")))
            (and (or (search "microsoft" version :test #'char-equal)
                     (search "wsl" version :test #'char-equal)
                     (uiop:getenv "WSL_DISTRO_NAME")
                     (uiop:getenv "WSL_INTEROP"))
                 t))))
  *wsl-session*)

(defun windows-path-p (token)
  "Is TOKEN shaped like a Windows path (`C:\\...`, `C:/...`)?  It matters
in a WSL session, where Explorer and Windows Terminal hand out paths in
Windows spelling for a filesystem evo sees mounted somewhere else."
  (and (>= (length token) 3)
       (alpha-char-p (char token 0))
       (char= (char token 1) #\:)
       (member (char token 2) '(#\\ #\/))))

(defun windows->wsl-path (path)
  "Map `C:\\Users\\a\\x.png` onto `/mnt/c/Users/a/x.png`.  NIL for a UNC
path (\\\\server\\share) or anything that is not drive-letter shaped: those
have no drive to mount, and guessing would hand the caller a wrong file."
  (let ((path (string-trim '(#\Space #\Tab #\Return #\Newline) path)))
    (when (windows-path-p path)
      (format nil "~a/~a/~{~a~^/~}"
              (string-right-trim "/" *wsl-mount-root*)
              (char-downcase (char path 0))
              (remove-if (lambda (part) (zerop (length part)))
                         (uiop:split-string (subseq path 3)
                                            :separator '(#\\ #\/)))))))

(defparameter *wsl-path-program* "wslpath"
  "The tool that maps Windows paths to Linux ones.  A parameter so a test
can take it away and exercise the fallback rule.")

(defun wsl-linux-path (windows-path)
  "The Linux path for a Windows one: ask wslpath, which knows the real
mount table, and fall back to the drive-letter rule when it is missing."
  (let ((mapped (and *wsl-path-program*
                     (run-child-output *wsl-path-program* (list "-u" windows-path)))))
    (or (and mapped (plusp (length mapped)) mapped)
        (windows->wsl-path windows-path))))

(defun windows-clipboard-script ()
  "PowerShell one-liner: save the clipboard's image to a temp PNG and print
`image:<path>`, or print `path:<path>` for a file copied in Explorer (the
Windows twin of Finder's «class furl»).  Exit 1 means neither was there."
  (concatenate
   'string
   "[Console]::OutputEncoding = [System.Text.Encoding]::UTF8; "
   "Add-Type -AssemblyName System.Windows.Forms; "
   "$img = Get-Clipboard -Format Image; "
   "if ($img -ne $null) { "
   "$p = [System.IO.Path]::ChangeExtension([System.IO.Path]::GetTempFileName(), 'png'); "
   "$img.Save($p, [System.Drawing.Imaging.ImageFormat]::Png); "
   "Write-Output ('image:' + $p); exit 0 }; "
   "$files = Get-Clipboard -Format FileDropList; "
   "if ($files -ne $null -and $files.Count -gt 0) { "
   "Write-Output ('path:' + $files[0]); exit 0 }; "
   "exit 1"))

(defun wsl-clipboard-image (dir)
  "Read the Windows clipboard from a WSL session.  Pixels are moved into
DIR (so they are cleaned up like any other scratch image); a file the
clipboard merely points at is returned where it lies, untouched."
  (when (wsl-p)
    ;; The first PowerShell that exists gets the question and its answer is
    ;; final: "the bridge ran and found nothing" is a real answer, and
    ;; asking three more spellings of the same interpreter would only cost
    ;; a second each to hear it again.
    (let ((result (loop for program in *wsl-powershell-programs*
                        when (evo.port:program-in-path program)
                          return (run-child-output program
                                                   (list "-NoProfile" "-Command"
                                                         (windows-clipboard-script))))))
      (when result
        (let* ((image (string-prefix-p "image:" result))
               (windows (cond (image (subseq result 6))
                              ((string-prefix-p "path:" result) (subseq result 5))))
               (mapped (and windows (wsl-linux-path windows)))
               (file (and mapped (ignore-errors (probe-file mapped)))))
          (when (and file (image-file-p file))
            (if image
                ;; Ours to own: move it out of the Windows temp directory
                ;; into the scratch dir, which the caller sweeps.
                (let ((out (merge-pathnames (format nil "clip-~a.png" (gen-id 8)) dir)))
                  (ignore-errors (uiop:copy-file file out))
                  (ignore-errors (delete-file file))
                  (and (image-file-p out) out))
                file)))))))

(defun windows-clipboard-image (dir)
  "Read the clipboard of the Windows evo is running *on* (not the one it is
running under, which is WSL-CLIPBOARD-IMAGE above).  Same PowerShell
question, no path mapping: the answer is already a native path."
  (when (evo.port:windows-p)
    (let ((result (loop for program in *wsl-powershell-programs*
                        when (evo.port:program-in-path program)
                          return (run-child-output program
                                                   (list "-NoProfile" "-Command"
                                                         (windows-clipboard-script))))))
      (when result
        (let* ((image (string-prefix-p "image:" result))
               (path (cond (image (subseq result 6))
                           ((string-prefix-p "path:" result) (subseq result 5))))
               (file (and path (ignore-errors (probe-file (string-trim '(#\Space #\Return #\Newline) path))))))
          (when (and file (image-file-p file))
            (if image
                ;; Ours to own: move it out of the Windows temp directory
                ;; into the scratch dir, which the caller sweeps.
                (let ((out (merge-pathnames (format nil "clip-~a.png" (gen-id 8)) dir)))
                  (ignore-errors (uiop:copy-file file out))
                  (ignore-errors (delete-file file))
                  (and (image-file-p out) out))
                file)))))))

(defvar *clipboard-readers*
  (list (cons "macOS pasteboard" #'macos-clipboard-image)
        (cons "wayland (wl-paste)" #'wayland-clipboard-image)
        (cons "X11 (xclip)" #'x11-clipboard-image)
        (cons "WSL (powershell.exe)" #'wsl-clipboard-image)
        (cons "Windows (powershell.exe)" #'windows-clipboard-image))
  "Ordered (NAME . FUNCTION) clipboard readers, tried in turn.  Rebind or
push onto this to teach evo a platform it does not ship support for; tests
inject a fake reader here rather than a real pasteboard.")

;;; Why nothing came back.
;;;
;;; "No image on the clipboard" is a lie when nothing in the session can
;;; read a clipboard at all — the user's screenshot IS on their clipboard,
;;; and evo blaming the clipboard sends them looking in the wrong place.
;;; The two cases are told apart by asking what this session is and which
;;; tools it has, and the answer names the missing piece.

(defun clipboard-gap (&key macos-p windows-p wsl-p wayland-p x11-p
                        (installed-p (constantly nil)))
  "Pure core of CLIPBOARD-REASON: the reason no reader could even try, or
NIL when one could (and the clipboard was simply imageless).  A session is
only in a gap when *none* of the tools that could serve it exist — a
Wayland desktop with xclip is served by XWayland, and WSLg bridges the
Windows clipboard into the Linux tools — so each case asks about every
reader that might have run, not just its own."
  (let ((unix-tools (or (funcall installed-p "wl-paste")
                        (funcall installed-p "xclip"))))
    (cond
      (macos-p (unless (funcall installed-p "osascript")
                 "no osascript on PATH — the macOS pasteboard is unreachable"))
      (windows-p (unless (some installed-p *wsl-powershell-programs*)
                   "no powershell on PATH — the Windows clipboard is unreachable"))
      (wsl-p (unless (or unix-tools (some installed-p *wsl-powershell-programs*))
               "no powershell.exe on PATH — the Windows clipboard is unreachable from WSL"))
      (wayland-p (unless unix-tools
                   "no clipboard tool here — install wl-clipboard for wl-paste"))
      (x11-p (unless (funcall installed-p "xclip")
               "no clipboard tool here — install xclip"))
      (t "this session has no clipboard (ssh or a bare tty)"))))

(defun clipboard-reason ()
  "Why CLIPBOARD-IMAGE found nothing, in the user's terms."
  (or (clipboard-gap :macos-p (uiop:os-macosx-p)
                     :windows-p (evo.port:windows-p)
                     :wsl-p (and (uiop:os-unix-p) (not (uiop:os-macosx-p)) (wsl-p))
                     :wayland-p (uiop:getenv "WAYLAND_DISPLAY")
                     :x11-p (uiop:getenv "DISPLAY")
                     :installed-p (lambda (program)
                                    (and (evo.port:program-in-path program) t)))
      "no image on the clipboard"))

(defun clipboard-image ()
  "Grab an image off the system clipboard as an :image block.
Returns (values BLOCK NIL) or (values NIL REASON)."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "evo-clip-~a" (gen-id 8))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (unwind-protect
         (loop for (nil . reader) in *clipboard-readers*
               for path = (ignore-errors (funcall reader dir))
               when path
                 return (attach-image-file
                         path
                         :name (if (uiop:subpathp path dir)
                                   (format nil "clipboard.~a"
                                           (or (pathname-type path) "png"))
                                   (file-namestring path))
                         :source "clipboard")
               finally
                  (return (values nil (if *clipboard-readers*
                                          (clipboard-reason)
                                          "no clipboard reader on this platform"))))
      (ignore-errors (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

;;; Pasted paths.
;;;
;;; Dragging a file onto a terminal (or copying a path) delivers the escaped
;;; path as a paste.  When a paste is nothing but paths and every one of them
;;; is an image, the gesture meant "attach these"; anything else is text and
;;; is left alone.  All-or-nothing on purpose: half-attaching a sentence that
;;; happens to mention a png would be a surprise, and surprises in an editor
;;; are worse than a feature not firing.

(defun split-shell-tokens (line)
  "Split LINE the way a shell would: backslash escapes and quotes, so
`/tmp/a\\ b.png` and `'/tmp/a b.png'` are each one token."
  (let ((tokens nil) (current (make-string-output-stream)) (any nil)
        (quote-char nil) (i 0) (n (length line)))
    (flet ((finish ()
             (let ((token (get-output-stream-string current)))
               (when (or any (plusp (length token)))
                 (push token tokens))
               (setf any nil))))
      (loop while (< i n)
            for char = (char line i)
            do (cond
                 ((and (char= char #\\) (< (1+ i) n) (null quote-char))
                  (write-char (char line (1+ i)) current)
                  (setf any t)
                  (incf i 2))
                 ((and quote-char (char= char quote-char))
                  (setf quote-char nil any t)
                  (incf i))
                 ((and (null quote-char) (member char '(#\" #\')))
                  (setf quote-char char any t)
                  (incf i))
                 ((and (null quote-char) (member char '(#\Space #\Tab)))
                  (finish)
                  (incf i))
                 (t (write-char char current)
                    (setf any t)
                    (incf i))))
      (finish))
    (nreverse tokens)))

(defun split-windows-tokens (line)
  "Split LINE into whitespace-separated tokens the way Windows does, honouring
double quotes but NOT treating backslash as an escape: on Windows (and for a
Windows path handed to a WSL session) the backslash is the path separator, so
the POSIX splitter would eat it and turn `C:\\Users\\a\\x.png` into
`C:Usersax.png`.  Single quotes are ordinary characters here, as they are to
cmd and PowerShell."
  (let ((tokens nil) (current (make-string-output-stream)) (any nil)
        (in-quote nil) (i 0) (n (length line)))
    (flet ((finish ()
             (let ((token (get-output-stream-string current)))
               (when (or any (plusp (length token)))
                 (push token tokens))
               (setf any nil))))
      (loop while (< i n)
            for char = (char line i)
            do (cond
                 ((char= char #\")
                  (setf in-quote (not in-quote) any t)
                  (incf i))
                 ((and (not in-quote) (member char '(#\Space #\Tab)))
                  (finish)
                  (incf i))
                 (t (write-char char current)
                    (setf any t)
                    (incf i))))
      (finish))
    (nreverse tokens)))

(defun percent-decode (string)
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length string))
          do (let ((char (char string i)))
               (if (and (char= char #\%) (< (+ i 2) (length string))
                        (digit-char-p (char string (+ i 1)) 16)
                        (digit-char-p (char string (+ i 2)) 16))
                   (progn (write-char (code-char (parse-integer string :start (1+ i)
                                                                       :end (+ i 3)
                                                                       :radix 16))
                                      out)
                          (incf i 3))
                   (progn (write-char char out) (incf i)))))))

(defun token->path (token)
  "Resolve a pasted token to a pathname: file:// URLs, ~ expansion,
Windows paths under WSL, and plain paths.  NIL when it cannot be a path at
all."
  (let ((token (string-trim '(#\Space #\Tab #\Return) token)))
    (cond
      ((zerop (length token)) nil)
      ;; A Windows path names a real file on Windows, and from inside WSL
      ;; through the mount table; anywhere else it is a string that happens
      ;; to have a colon in it.
      ((windows-path-p token)
       (cond ((evo.port:windows-p) token)
             ((wsl-p) (wsl-linux-path token))))
      ((string-prefix-p "file://" token)
       (let ((rest (subseq token 7)))
         ;; file://localhost/x and file:///x both mean /x
         (when (string-prefix-p "localhost" rest) (setf rest (subseq rest 9)))
         (percent-decode rest)))
      ((or (string-prefix-p "/" token) (string-prefix-p "~" token)
           (string-prefix-p "./" token) (string-prefix-p "../" token))
       (namestring (uiop:parse-native-namestring
                    (if (string-prefix-p "~" token)
                        (concatenate 'string
                                     (string-right-trim "/" (namestring (user-homedir-pathname)))
                                     (subseq token 1))
                        token))))
      (t token))))

(defun strip-quotes (token)
  "TOKEN without a matching pair of surrounding quotes."
  (let ((n (length token)))
    (if (and (>= n 2)
             (member (char token 0) '(#\" #\'))
             (char= (char token (1- n)) (char token 0)))
        (subseq token 1 (1- n))
        token)))

(defun pasted-image-paths (text)
  "The image files TEXT names, if TEXT is nothing but paths to images.
NIL for ordinary text — including text that merely mentions an image path."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         ;; On Windows (and for a Windows path in a WSL session) the tokenizer
         ;; must keep backslashes: they are path separators, not escapes.
         (windows-mode (or (evo.port:windows-p) (wsl-p))))
    ;; A single quoted-or-bare Windows path may hold spaces and backslashes
    ;; and must skip the splitter entirely — POSIX quoting reads
    ;; `C:\Users\a\shot.png` as escapes and hands back `C:Usersashot.png`, and
    ;; the splitter would break an unquoted path at its spaces.  Only claim it
    ;; when it actually resolves to an image; otherwise fall through, so
    ;; several paths (or a path plus prose) still get the multi-token pass.
    (let ((windows (strip-quotes trimmed)))
      (when (windows-path-p windows)
        (let* ((path (token->path windows))
               (file (and path (ignore-errors (probe-file path)))))
          (when (and file (image-file-p file))
            (return-from pasted-image-paths (list file))))))
    (let ((tokens (loop for line in (uiop:split-string trimmed :separator '(#\Newline))
                        append (funcall (if windows-mode
                                            #'split-windows-tokens
                                            #'split-shell-tokens)
                                        (string-trim '(#\Return) line)))))
      (when (and tokens (<= (length tokens) 16))
        (loop for token in tokens
              for path = (token->path token)
              for file = (and path (ignore-errors (probe-file path)))
              unless (and file (image-file-p file))
                do (return nil)
              collect file)))))
