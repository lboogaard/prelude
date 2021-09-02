;;; lab-keybindings-functions.el
;;
;; Additional keybindings and functions
;;
;; Written/adapted by Leindert Boogaard
;;
;; Note: these may overwrite some prelude bindings
;;
;; Note: (some of) these need to be defined in the prelude-mode-map,
;; not the global map, otherwise they won't work


;;; Keybindings

;; a secondary key for magit-status
(define-key prelude-mode-map (kbd "<f10>") 'magit-status)

;; note: overwrites swap-super-and-meta
(define-key prelude-mode-map (kbd "C-c w") 'delete-trailing-whitespace)

;; note: overwrites crux-visit-term-buffer
(define-key prelude-mode-map (kbd "C-c t") 'toggle-truncate-lines)

;; useful besides helm-do-grep-ag
(define-key helm-command-map (kbd "M-s a") 'helm-do-ag)

;; consider binding some smartparens functions
;; https://ebzzry.com/en/emacs-pairs/
(define-key prelude-mode-map (kbd "M-[") 'sp-backward-unwrap-sexp)
(define-key prelude-mode-map (kbd "M-]") 'sp-unwrap-sexp)

;;; Functions

;; Open path in iTerm2
(defun lab/iterm-here ()
  (interactive)
  (dired-smart-shell-command "open -a iTerm $PWD" nil nil))

(define-key global-map (kbd "M-0") 'lab/iterm-here)


;; Flip bool at point
;;A list of opposite boolean pairs.
(defvar bools '(("true" . "false") ("True" . "False") ("TRUE" . "FALSE")
                ("#t" . "#f") ("yes" . "no") ("Yes" . "No")))

(defun lab/flip-bool-at-point ()
  "Flips the boolean literal at point, changing true to false and
vice-versa."
  (interactive)
  (let* ((true  (cdr (assoc  (current-word) bools)))
         (false (car (rassoc (current-word) bools)))
         (wrd (cond (true true)
                    (false false)
                    (t (current-word)))))
    (save-excursion
      (forward-word)
      (backward-kill-word 1)
      (insert wrd))))

;; C-c C-b overwrites prelude's eval-buffer, let's try something else
(global-set-key (kbd "C-c C-b") 'lab/flip-bool-at-point)


;; Convenient regexes to align:
;; Source: http://pragmaticemacs.com/emacs/aligning-text/
(defun lab/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun lab/align-& (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))

(defun lab/align-comma (start end)
  "Align columns by comma"
  (interactive "r")
  (align-regexp start end
                ",\\(\\s-*\\)" 1 1 t))

(defun lab/align-point (start end)
  "Align columns by comma"
  (interactive "r")
  (align-regexp start end
                ".\\(\\s-*\\)" 1 1 t))

(defun lab/align-plus (start end)
  "Align columns by comma"
  (interactive "r")
  (align-regexp start end
                "+\\(\\s-*\\)" 1 1 t))


(defun lab/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice.
source: https://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html"
  (interactive)
  (let ((fill-column
         (if (eq last-command 'lab/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'lab/fill-or-unfill)


(defun lab/round-numbers-3dp (start end)
"Round the numbers in region to three decimal places."
(interactive "r")
(save-restriction
  (narrow-to-region start end)
  (goto-char 1)
  (let ((case-fold-search nil))
    (while (search-forward-regexp "\\([0-9]+\\.[0-9]+\\)" nil t)
      (replace-match (format "%0.3f" (string-to-number (match-string 1)))
                     )))))

(defun lab/round-numbers-2dp (start end)
"Round the numbers in region to three decimal places."
(interactive "r")
(save-restriction
  (narrow-to-region start end)
  (goto-char 1)
  (let ((case-fold-search nil))
    (while (search-forward-regexp "\\([0-9]+\\.[0-9]+\\)" nil t)
      (replace-match (format "%0.2f" (string-to-number (match-string 1)))
                     )))))


;; quick spell checking of text in Dutch/English (British/American)
(define-prefix-command 'lab/flyspell-map)

(define-key ctl-x-map "y" lab/flyspell-map)

;; tried to write some functions, can probably be simplified
(defun lab/check-buffer (lang)
  (interactive)
  (ispell-change-dictionary lang)
  (flyspell-buffer))

(defun lab/check-buffer-dutch ()
  (interactive)
  (lab/check-buffer "dutch"))

(defun lab/check-buffer-british ()
  (interactive)
  (lab/check-buffer "british"))

(defun lab/check-buffer-american ()
  (interactive)
  (lab/check-buffer "american"))

(define-key lab/flyspell-map (kbd "d") 'lab/check-buffer-dutch)
(define-key lab/flyspell-map (kbd "b") 'lab/check-buffer-british)
(define-key lab/flyspell-map (kbd "a") 'lab/check-buffer-american)
