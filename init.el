;; Cosmetic stuff
(setq inhibit-splash-screen t)
(setq frame-title-format "%b")
(setq icon-title-format "%b")
(when window-system
  (tool-bar-mode -1)
  (set-frame-position (selected-frame) 250 30)
  (set-frame-height (selected-frame) 60)
  (set-frame-width (selected-frame) 130))

(setq show-trailing-whitespace t)

(eval-when-compile (require 'cl))

(defun toggle-transparency ()
  (interactive)
  (let ((param (cadr (frame-parameter nil 'alpha))))
    (if (and param (/= param 100))
        (set-frame-parameter nil 'alpha '(100 100))
      (set-frame-parameter nil 'alpha '(85 50)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Turn on autoindent, I guess
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Set font
(set-face-attribute 'default nil :font "Source Code Pro-13")

;; Load random stuff
(add-to-list 'load-path "~/.emacs.d/non-elpa/")
(require 'eieio)

;; Packages
(require 'package)
(add-to-list 'package-archives '("marmalade" .
                                 "http://marmalade-repo.org/packages/")
             t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Linum
(require 'linum)
(global-linum-mode)

;; Backups
(setq backup-directory-alist `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms `((".*", "~/.autosaves")))

;; Refheap

(defun get-string-from-file (file-path)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(when (file-exists-p "~/.refheapcreds")
  (let ((creds (read (get-string-from-file "~/.refheapcreds"))))
    (setq refheap-token (cdr (assoc 'token creds))
          refheap-user (cdr (assoc 'user creds)))))

;; Ugly generated stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-delay 0.1)
 '(ac-disable-inline nil)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "open")
 '(clojure-defun-indents (quote (lazy-loop cond-let GET POST PATCH PUT element catch-exception-string let-programs fact facts)))
 '(custom-safe-themes (quote ("32ff89088ee3518fc03954b09ecca3614fdc51aa5108dfb3b3cba68083b701f6" "344ff60900acf388116822a0540b34699fc575cf29a5c9764453d045cc42a476" "935e766f12c5f320c360340c8d9bc1726be9f8eb01ddeab312895487e50e5835" "cfd71d55f448690641d6e8ca6438ab696bcaff3296905f95d91d4990166863d5" "ca2d69f5dd853dbf6fbcf5d0f1759ec357fda19c481915431015417ec9c1fbd8" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(erc-modules (quote (completion log spelling track hl-nicks netsplit button match track readonly networks ring autojoin noncommands irccontrols move-to-prompt stamp menu list)))
 '(erc-track-exclude-types (quote ("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477")))
 '(erc-track-showcount nil)
 '(erc-track-switch-direction (quote newest))
 '(evil-default-cursor (quote (t)))
 '(fill-column 80)
 '(indent-tabs-mode nil)
 '(ispell-dictionary nil)
 '(ispell-local-dictionary nil)
 '(ispell-program-name "/usr/local/bin/aspell")
 '(js-indent-level 2)
 '(mouse-autoselect-window t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.trampauto")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'emacs-lisp-mode-hook
          (lambda () (paredit-mode t)))

(ido-mode)

;; Show some whitespace.

(setq show-trailing-whitespace t)

;; Stop dragging me down, man.
(setq mouse-drag-copy-region nil)

;; Default to ssh and stop asking me stupid questions.
(setq tramp-default-method "ssh")

;; Add some extensions for markdown mode.
(setq auto-mode-alist
      (append '(("\\.md" . markdown-mode)
                ("\\.markdown" . markdown-mode))
              auto-mode-alist))

;; Add some extensions for Scala mode.
(setq auto-mode-alist
      (append '(("\\.scala" . scala-mode))
              auto-mode-alist))

;; Path fix for OS X.
;; (if (not (getenv "TERM_PROGRAM"))
;;       (let ((path (shell-command-to-string
;;               "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
;;         (setenv "PATH" path)))

;; (add-to-list 'exec-path "~/.cabal/bin") ;; because fuck
;; (add-to-list 'exec-path "~/")

;; Clojure mode and cider
(add-to-list 'auto-mode-alist '("\\.cljs" . clojure-mode))
(eval-after-load 'clojure-mode
  '(progn
     (setq clojure-mode-use-backtracking-indent t)
     (add-hook 'clojure-mode-hook
               (lambda ()
                 (paredit-mode t)
                 (show-paren-mode t)
                 (put-clojure-indent 'fact 'defun)
                 (put-clojure-indent 'prepend 'defun)
                 (put-clojure-indent 'when-short 'defun)))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode
    `(("(\\(fn\\>\\)"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1)
                                 ,(make-char 'greek-iso8859-7 107))
                 nil))))))

;; Keybindings
(global-set-key (kbd "C-c r") 'refheap-paste-region)
(global-set-key (kbd "C-c b") 'refheap-paste-buffer)
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "<s-mouse-1>") 'flyspell-correct-word)
(global-set-key (kbd "C-c f") 'finder)
(global-set-key (kbd "C-c l") 'goto-line)


;; Stolen stuff from emacs starter kit

(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(setq-default indent-tabs-mode nil)

;; Evil mode
(add-to-list 'load-path "~/.emacs.d/non-elpa/evil")
(require 'evil)
(evil-mode 1)

;; asciidoc
(add-to-list 'load-path "~/.emacs.d/non-elpa/adoc-mode")
(add-to-list 'load-path "~/.emacs.d/non-elpa/markup-faces")
(require 'adoc-mode)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/non-elpa/tomorrow-night")
(load-theme 'tomorrow-night t)

;; Sane undo and redo
(global-undo-tree-mode)

;; Line breaks while committing
(add-hook 'magit-log-edit-mode 'auto-fill-mode)

(add-to-list 'load-path "~/.emacs.d/non-elpa/cider")
(require 'cider)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Haskell

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Starting a server
(server-start)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook
          (lambda ()
            (set-syntax-table clojure-mode-syntax-table)
            (setq lisp-indent-function 'clojure-indent-function)))
(setq cider-repl-popup-stacktraces t)
(setq cider-repl-print-length 100)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Go

(eval-after-load "go-mode"
  '(progn
     (require 'flymake-go)
     (add-hook 'before-save-hook #'gofmt-before-save)))


;; Because fuck

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Javascript

(setq js-indent-level 4)

;; Show column info in modeline

(setq column-number-mode t)

(elpy-enable)
