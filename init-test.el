;; Minimal init to load move-mode and test that it works in a vanilla
;; Emacs.  Run with:
;;
;;     $ env HOME=${TMPDIR:-/tmp} emacs -Q --load init-test.el
;;
;; To load `move-mode' and open an example file.

(let ((repo-root (file-name-directory load-file-name)))
  (add-to-list 'load-path (file-name-directory load-file-name))
  (require 'move-mode)

  (find-file (concat repo-root "test/sources/example.move")))
