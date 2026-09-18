;; I run into the c-stack overflow bug too frequently,
;; let's try something to fix it:
;; see: https://github.com/hlissner/doom-emacs/issues/1407#issuecomment-491931901
(setq undo-limit 40000           ;; 160000  defaults
      undo-outer-limit 8000000   ;; 24000000
      undo-strong-limit 100000)  ;; 240000
