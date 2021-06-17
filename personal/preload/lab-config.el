;; lab-config.el
;;
;; Additional packages that are preloaded (before prelude)
;; mainly theme(s)
;;
;; Written by Leindert Boogaard

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;
(use-package solarized-theme
             :ensure t
             :if window-system
             :init
             ;; Make the modeline high contrast
             (setq solarized-high-contrast-mode-line t)
             ;; Underline below bottomline (not baseline) which is lower (better readable)
             (setq x-underline-at-descent-line t)
             ;; Don't change the font for some headings and titles
             (setq solarized-use-variable-pitch nil)
             ;; Don't change size of org-mode headlines (but keep other size-changes)
             (setq solarized-scale-org-headlines nil)
             ;; ;; Use less colors for indicators such as git:gutter, flycheck and similar
             (setq solarized-emphasize-indicators nil)
             ;; ;; Use less bolding
             ;; (setq solarized-use-less-bold t)
             ;; ;; Use more italics
             ;; (setq solarized-use-more-italic t)
             :config
             ;;(load-theme 'solarized-light t)
             ;;(load-theme 'solarized-dark t)

             (setq prelude-theme 'solarized-light)
             )
