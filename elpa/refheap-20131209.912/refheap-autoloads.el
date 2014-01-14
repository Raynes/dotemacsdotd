;;; refheap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (refheap-paste-buffer-private refheap-paste-buffer
;;;;;;  refheap-paste-region-private refheap-paste-region) "refheap"
;;;;;;  "refheap.el" (21183 4369 0 0))
;;; Generated autoloads from refheap.el

(autoload 'refheap-paste-region "refheap" "\
Paste the current region to refheap. With prefix arg, paste privately.

\(fn BEGIN END &optional PRIVATE)" t nil)

(autoload 'refheap-paste-region-private "refheap" "\
Paste the current region to a private refheap entry.

\(fn BEGIN END)" t nil)

(autoload 'refheap-paste-buffer "refheap" "\
Paste the current buffer to refheap. With prefix arg, paste privately.

\(fn &optional PRIVATE)" t nil)

(autoload 'refheap-paste-buffer-private "refheap" "\
Paste the current buffer to a private refheap entry.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("refheap-pkg.el") (21183 4369 868048 0))

;;;***

(provide 'refheap-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; refheap-autoloads.el ends here
