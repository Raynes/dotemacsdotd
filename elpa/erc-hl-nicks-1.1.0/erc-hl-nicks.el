;;; erc-hl-nicks.el --- ERC nick highlighter that ignores uniquifying chars when colorizing

;; Copyright (C) 2011  David Leatherman

;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/erc-hl-nicks
;; Version: 1.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file was originally erc-highlight-nicknames.  It was modified
;; to optionally ignore the uniquifying characters that IRC clients
;; add to nicknames

;; History

;; 1.1.0 - Remove use of cl package (was using 'reduce').
;;       - The hook is called with a narrowed buffer, so it makes
;;         more sense to iterate over each word, one by one.  This
;;         is more efficient and has a secondary benefit of fixing a
;;         case issue.
;;       - Added an option to not highlight fools

;; 1.0.4 - Use erc-channel-users instead of erc-server-users
;;       - Ignore leading characters, too.

;; 1.0.3 - Was finding but not highlighting nicks with differing
;;         cases. Fixed. Ignore leading characters, too. Doc changes.

;; 1.0.2 - Fixed a recur issue, prevented another, and fixed a
;;         spelling issue.

;; 1.0.1 - tweaked so that the re-search will pick up instances of the
;;         trimmed nick, settled on 'nick' as the variable name
;;         instead of kw, keyword, word, etc

;; 1.0.0 - initial release

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'erc)
(require 'erc-button)

(defgroup erc-hl-nicks nil
  "Highlighting nicknames in erc buffers"
  :group 'erc)

(defcustom erc-hl-nicks-trim-nick-for-face t
  "Ignore some characters when determining nick face"
  :group 'erc-hl-nicks
  :type 'boolean)

(defcustom erc-hl-nicks-ignore-chars ",`'_"
  "Characters at the end of a nick to ignore while highlighting"
  :group 'erc-hl-nicks
  :type 'string)

(defcustom erc-hl-nicks-ignore-case nil
  "Ignore case when searching for nicks to highlight"
  :group 'erc-hl-nicks
  :type 'boolean)

(defcustom erc-hl-nicks-highlight-fools nil
  "Don't highlight the nicks for people in the erc-fools list"
  :group 'erc-hl-nicks
  :type 'boolean)

(defface erc-hl-nicks-nick-base-face
  '((t nil))
  "Base face used for highlighting nicks. (Before the nick
  color is added)"
  :group 'erc-hl-nicks)

(defvar erc-hl-nicks-face-table
  (make-hash-table :test 'equal)
  "The hash table that contains unique nick faces.")

;; for debugging
(defun erc-hl-nicks-reset-face-table ()
  (setq erc-hl-nicks-face-table
        (make-hash-table :test 'equal)))

(defun erc-hl-nicks-hexcolor-luminance (color)
  "Returns the luminance of color COLOR. COLOR is a string \(e.g.
  \"#ffaa00\", \"blue\"\) `color-values' accepts. Luminance is a
  value of 0.299 red + 0.587 green + 0.114 blue and is always
  between 0 and 255."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (car (cdr values)))
         (b (car (cdr (cdr values)))))
    (floor (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)) 256)))

(defun erc-hl-nicks-invert-color (color)
  "Returns the inverted color of COLOR."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (car (cdr values)))
         (b (car (cdr (cdr values)))))
    (format "#%04x%04x%04x"
            (- 65535 r) (- 65535 g) (- 65535 b))))

(defun erc-hl-nicks-trim-irc-nick (nick)
  "Removes instances of erc-hl-nicks-ignore-chars from both sides of the nick"
  (let ((stripped (replace-regexp-in-string
                   (format "\\([%s]\\)+$" erc-hl-nicks-ignore-chars)
                   "" nick)))
    (replace-regexp-in-string
     (format "^\\([%s]\\)+" erc-hl-nicks-ignore-chars)
     "" stripped)))

(defun erc-hl-nicks-color-for-nick (nick)
  "Get the color to use for the given nick"
  (let ((color (concat "#" (substring (md5 (downcase nick)) 0 12)))
        (bg-mode (cdr (assoc 'background-mode (frame-parameters)))))
    (cond
     ((and (equal 'dark bg-mode)
           (< (erc-hl-nicks-hexcolor-luminance color) 85))
      (erc-hl-nicks-invert-color color))
     ((and (equal 'light bg-mode)
           (> (erc-hl-nicks-hexcolor-luminance color) 170))
      (erc-hl-nicks-invert-color color))
     (t color))))

(defun erc-hl-nicks-make-face (nick)
  "Create and cache a new face for the given nick"
  (or (gethash nick erc-hl-nicks-face-table)
      (let ((color (erc-hl-nicks-color-for-nick nick))
            (new-nick-face
             (make-symbol (concat "erc-hl-nicks-nick-" nick "-face"))))
        (copy-face 'erc-hl-nicks-nick-base-face new-nick-face)
        (set-face-foreground new-nick-face color)
        (puthash nick new-nick-face erc-hl-nicks-face-table))))

(defun erc-hl-nicks-highlight-fool-p (nick)
  (if (member nick erc-fools)
      erc-hl-nicks-highlight-fools
      t))

;;;###autoload
(defun erc-hl-nicks ()
  "Retrieves a list of usernames from the server and highlights them"
  (save-excursion
    (with-syntax-table erc-button-syntax-table
      (goto-char (point-min))
      (while (forward-word 1)
        (let* ((word (word-at-point))
               (trimmed (erc-hl-nicks-trim-irc-nick word))
               (bounds (bounds-of-thing-at-point 'word))
               (inhibit-read-only t))
          (when (and erc-channel-users
                     (erc-get-channel-user word)
                     (erc-hl-nicks-highlight-fool-p trimmed))
            (erc-button-add-face (car bounds) (cdr bounds)
                                 (erc-hl-nicks-make-face trimmed))))))))

(define-erc-module hl-nicks nil
  "Highlight usernames in the buffer"
  ((add-hook 'erc-insert-modify-hook 'erc-hl-nicks t))
  ((remove-hook 'erc-insert-modify-hook 'erc-hl-nicks)))

;;;###autoload
(eval-after-load 'erc '(add-to-list 'erc-modules 'hl-nicks))

;; For first time use
;;;###autoload
(when (and (boundp 'erc-modules)
           (not (member 'hl-nicks 'erc-modules)))
  (add-to-list 'erc-modules 'hl-nicks))

(provide 'erc-hl-nicks)
;;; erc-hl-nicks.el ends here
