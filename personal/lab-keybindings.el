;; Custom keybindings for Leindert Boogaard
;; These may overwrite some prelude bindings

;; Note that (some of?) these need to be defined in the
;; prelude-mode-map, not the global map, otherwise they won't work

;; the right key for magit-status
(global-set-key (kbd "<f10>") 'magit-status)

;; very useful; overwrites swap-super-and-meta
(define-key prelude-mode-map (kbd "C-c w") 'delete-trailing-whitespace)

;; very useful; overwrites crux-visit-term-buffer
(define-key prelude-mode-map (kbd "C-c t") 'toggle-truncate-lines)
