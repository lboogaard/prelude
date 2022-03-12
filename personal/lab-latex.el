;; texcount word count (note: needs the extra space behind additional options)
;; http://superuser.com/questions/125027/word-count-for-latex-within-emacs
;; Note that the number of words in a region can be counted with M-= in any mode.
(defun lab/latex-word-count ()
  (interactive)
  (shell-command (concat "texcount " ;; is installed with LaTeX in /usr/texbin/texcount
			 "-total "
                         "-unicode "
                         "-inc "
                         (shell-quote-argument buffer-file-name))))
(add-hook 'LaTeX-mode-hook (lambda () (define-key LaTeX-mode-map "\C-cw" 'lab/latex-word-count)))

;; Replace in math mode -  http://tex.stackexchange.com/a/65340/81077
(defun lab/latex-query-replace-mathvar (a b)
  (interactive "sfrom: \nsto: ")
  (beginning-of-buffer)
  (while (re-search-forward
	  "\\(\\\\(\\|\\\\\\[\\|[^\\\\]\$\$?\\|\\\\begin{equation}\\|\\\\begin{align}\\)" nil 1)
    (query-replace-regexp a  b t  (point)
			  (progn (re-search-forward
				  "\\(\\\\)\\|\\\\\\]\\|[^\\\\]\$\$?\\|\\\\end{equation}\\|\\\\end{align}\\)" nil 1) (point)))))

;; latexmk document compilation - http://tex.stackexchange.com/q/10561
(add-hook 'TeX-mode-hook (lambda ()
                           (add-to-list 'TeX-command-list '("LaTeX Make" "latexmk -pdf -f %t" TeX-run-TeX))
                           (add-to-list 'TeX-command-list '("LaTeX Make Bg" "latexmk -pvc -pdf -view=none %t" TeX-run-TeX))
                           (add-to-list 'TeX-command-list '("LuaLaTeX Make" "latexmk -lualatex -pdflua -f %t" TeX-run-TeX))
                           (add-to-list 'TeX-command-list '("LuaLaTeX Make Bg" "latexmk -pvc -lualatex -pdflua -view=none %t" TeX-run-TeX))
                           (setq TeX-command-default "LaTeX Make")))

;; Use Skim as pdf viewer (system default may be preview):
(setq TeX-view-program-selection
      '((output-pdf "Skim")))
(setq TeX-view-program-list
      '(("Skim" "open -a skim %o")))


;; Turn on RefTeX andn plugin to AUCTeX
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;; Set my preferred citation styles
(setq reftex-cite-format 'natbib)
(setq reftex-ref-style-default-list '("Default" "Hyperref"))

;; modify the e.g., in natbib https://tex.stackexchange.com/a/631387/81077)
(setf (cdr (assoc ?e (caddr (assoc 'natbib reftex-cite-format-builtin))))
      "\\citep[e.g.,][]{%l}")
