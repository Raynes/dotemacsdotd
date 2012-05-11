;; Cosmetic stuff
(setq inhibit-splash-screen t)
(setq frame-title-format "%b")
(setq icon-title-format "%b")
(when window-system
  (tool-bar-mode -1)
  (set-frame-position (selected-frame) 250 30)
  (set-frame-height (selected-frame) 60)
  (set-frame-width (selected-frame) 130))

(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (let ((param (cadr (frame-parameter nil 'alpha))))
    (if (and param (/= param 100))
        (set-frame-parameter nil 'alpha '(100 100))
      (set-frame-parameter nil 'alpha '(85 50)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Set font
(set-face-attribute 'default nil :family "Anonymous Pro" :height 130)

;; Load random stuff
(add-to-list 'load-path "~/.emacs.d/non-elpa/")
(require 'eieio)

;; Packages
(require 'package)
(add-to-list 'package-archives '("marmalade" .
                                 "http://marmalade-repo.org/packages/")
             t)
(package-initialize)

;; Linum
(require 'linum)
(global-linum-mode)

;; Backups
(setq backup-directory-alist `(("." . "~/.saves")))

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
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "open")
 '(custom-safe-themes (quote ("6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(erc-modules (quote (completion log spelling track hl-nicks netsplit button match track readonly networks ring autojoin noncommands irccontrols move-to-prompt stamp menu list)))
 '(erc-track-exclude-types (quote ("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477")))
 '(erc-track-showcount nil)
 '(erc-track-switch-direction (quote newest))
 '(evil-default-cursor (quote (t)))
 '(fringe-mode (quote (4 . 4)) nil (fringe))
 '(indent-tabs-mode nil)
 '(ispell-dictionary nil)
 '(ispell-local-dictionary nil)
 '(ispell-program-name "/usr/local/bin/aspell")
 '(mouse-autoselect-window t)
 '(org-startup-indented t)
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
(require 'whitespace)
(setq whitespace-style '(trailing tabs))

(setq mouse-drag-copy-region nil)

(setq tramp-default-method "ssh")

;; Add some extensions for markdown mode.
(setq auto-mode-alist
      (append '(("\\.md" . markdown-mode)
                ("\\.markdown" . markdown-mode))
              auto-mode-alist))

;; Path fix for OS X.
(setenv "PATH" (shell-command-to-string "source ~/.bashrc && echo $PATH"))

;; Clojure mode
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

;; Keybindings
(global-set-key (kbd "C-c r") 'gist-region)
(global-set-key (kbd "C-c b") 'gist-buffer)
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c j") 'clojure-jack-in)
(global-set-key (kbd "<s-mouse-1>") 'flyspell-correct-word)
(global-set-key (kbd "C-c f") 'finder)
(global-set-key (kbd "C-c l") 'goto-line)

;; Starting a server
(server-start)

;; Setting the swank command.
(setq clojure-swank-command "lein jack-in %s")

;; ERC
(eval-after-load 'znc
  '(progn
     (require 'todochiku)
     (global-set-key (kbd "C-c z") 'znc-all)

     (defun highlight-me (match-type nick message)
       (unless (posix-string-match "^\\** Users on #" message)
         (todochiku-message
          (concat "ERC: Highlight on " (buffer-name (current-buffer)))
          (concat "<" (first (split-string nick "!")) "> " message)
          "")))

     (add-hook 'erc-text-matched-hook 'highlight-me)
     (add-hook 'erc-text-matched-hook 'erc-beep-on-match)

     (setq erc-track-exclude-types 
           '("JOIN" "NICK" "PART" "QUIT" "MODE" "KICK"
             "324" "329" "332" "333" "353" "477")
           erc-track-switch-direction 'newest
           erc-track-exclude-server-buffer t
           erc-log-channels-directory "~/.erc/logs/"
           znc-servers (read (get-string-from-file "~/.ercznc")))

     (erc-spelling-mode 1)
     (erc-log-mode 1)

     (defun current-itunes-song ()
       (do-applescript
        "tell application \"iTunes\"
       set artist_name to the artist of the current track
       set song_title to the name of the current track
       set song_album to the album of the current track
       set song_length to the time of the current track
       set played_count to the played count of the current track
       return song_title & \" - \" & artist_name & \" [\" & song_album & \"] [length: \" & song_length & \"] [played: \" & played_count & \"]\"
    end tell"))

     (defun erc-cmd-NP ()
       (erc-cmd-ME (concat "" (current-itunes-song))))

     (defun PRIVMSG-notify (proc parsed)
       (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
             (target (car (erc-response.command-args parsed)))
             (msg (erc-response.contents parsed)))
         (when (and (erc-current-nick-p target) 
                    (not (erc-is-message-ctcp-and-not-action-p msg)))
           (todochiku-message
            (concat "ERC: Direct message from " nick)
            msg
            ""))))

     (add-hook 'erc-server-PRIVMSG-functions 'PRIVMSG-notify)))

;; Stolen stuff from emacs starter kit

(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(eval-after-load 'js
  '(progn (define-key js-mode-map "{" 'paredit-open-curly)
          (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
          (add-hook 'js-mode-hook 'esk-paredit-nonlisp)
          (setq js-indent-level 2)
          ;; fixes problem with pretty function font-lock
          (define-key js-mode-map (kbd ",") 'self-insert-command)
          (font-lock-add-keywords
           'js-mode `(("\\(function *\\)("
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "\u0192")
                                 nil)))))))

;; Evil mode
(add-to-list 'load-path "~/.emacs.d/non-elpa/evil")
(require 'evil)
(evil-mode 1)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/non-elpa/emacs-color-theme-solarized")
(load-theme 'solarized-dark t)

;; Sane undo and redo
(global-undo-tree-mode)

;; Line breaks while committing
(add-hook 'magit-log-edit-mode 'auto-fill-mode)
