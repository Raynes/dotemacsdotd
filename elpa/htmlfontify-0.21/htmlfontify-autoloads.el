;;; htmlfontify-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (htmlfontify-copy-and-link-dir htmlfontify-buffer)
;;;;;;  "htmlfontify" "htmlfontify.el" (20180 62980))
;;; Generated autoloads from htmlfontify.el

(autoload 'htmlfontify-buffer "htmlfontify" "\
Create a new buffer, named for the current buffer + a .html extension,
containing an inline CSS-stylesheet and formatted CSS-markup HTML
that reproduces the look of the current Emacs buffer as closely
as possible.

Dangerous characters in the existing buffer are turned into HTML
entities, so you should even be able to do HTML-within-HTML
fontified display.

You should, however, note that random control or eight-bit
characters such as ^L () or \244 (\244) won't get mapped yet.

If the SRCDIR and FILE arguments are set, lookup etags derived
entries in the `hfy-tags-cache' and add HTML anchors and
hyperlinks as appropriate.

\(fn &optional SRCDIR FILE)" t nil)

(autoload 'htmlfontify-copy-and-link-dir "htmlfontify" "\
Trawl SRCDIR and write fontified-and-hyperlinked output in DSTDIR.
F-EXT and L-EXT specify values for `hfy-extn' and `hfy-link-extn'.

You may also want to set `hfy-page-header' and `hfy-page-footer'.

\(fn SRCDIR DSTDIR &optional F-EXT L-EXT)" t nil)

;;;***

;;;### (autoloads nil nil ("htmlfontify-pkg.el") (20180 62980 954199))

;;;***

(provide 'htmlfontify-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; htmlfontify-autoloads.el ends here
