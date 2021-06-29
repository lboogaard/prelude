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


;; Theme
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

;; Fancy titlebar for MacOS matching the colorscheme
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Font
;; (set-frame-font "Menlo 12")                ; default
(set-frame-font "Hack 12")
;; (set-frame-font "Source Code Pro 12")
;; (set-frame-font "Input Mono Narrow 12")
;; (set-frame-font "Fira Code 12")


;; Additional Prelude modifications:
(setq prelude-whitespace nil)



;; Finally, start server if not running as deamon
;;------------------------------------------------------------------------------
;; Start server
;;------------------------------------------------------------------------------
;; Better solution is to start an emacs-server and open files with emacsclient
;; Finder now works with this and .bashrc contains and an alias Emacs to
;; emacsclient (with some tricks)
(use-package server
  :if window-system ;; don't start server in terminal
  :config
  (unless (server-running-p) (server-start)))
