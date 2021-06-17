;;; lab-modules.el
;;
;; Custom packages not included in prelude
;;
;; Leindert Boogaard


;; http://endlessparentheses.com/multiple-cursors-keybinds.html
(define-prefix-command 'lab/mc-map)
;; C-x m is usually `compose-mail'. Bind it to something
;; else if you use this command.
(define-key ctl-x-map "m" 'lab/mc-map)

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M-<" . mc/mark-all-like-this)
         ("C-+" . mc/insert-numbers)
         ;; ("C-S-c C-S-c" . mc/edit-lines)

         :map ctl-x-map
         ;; This is globally useful, so it goes under `C-x', and `m' for
         ;; "multiple-cursors" is easy to remember.
         ("a" . mc/mark-all-dwim)
         ;; Usually, both `C-x C-m' and `C-x RET' invoke the `mule-keymap',
         ;; but that's a waste of keys. Here we put it just under `C-x RET'.
         ("<return>" . mule-keymap)

         :map lab/mc-map
         ;;; Really really nice!
         ("i" . mc/insert-numbers)
         ("h" . mc-hide-unmatched-line)
         ("A" . mc/mark-all-like-this)
         ;;; Occasionally useful
         ("d" . mc/mark-all-symbols-like-this-in-defun)
         ("r" . mc/reverse-regions)
         ("s" . mc/sort-regions)
         ("l" . mc/edit-lines)
         ("\C-a" . mc/edit-beginnings-of-lines)
         ("\C-e" . mc/edit-ends-of-lines))
  )


;; Reveal in OSX finder: http://stackoverflow.com/a/21560302/5064815
(use-package reveal-in-osx-finder
  :ensure t
  :config
  ;; This command makes total sense paired with dired:
  (global-set-key (kbd "C-x f") 'reveal-in-osx-finder)
  ;; Rebind this then:
  (global-set-key (kbd "C-x \S-f") 'set-fill-column)
  )
