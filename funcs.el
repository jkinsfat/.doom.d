;;; ~/.doom.d/funcs.el -*- lexical-binding: t; -*-
(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'org-babel-tangle :append :local)))

(defun my/open-literate-private-config-file ()
  "Open the private config.org file."
  (interactive)
  (find-file (expand-file-name "config.org" doom-private-dir)))

(defun my/open-org-files-list ()
  (delq nil
        (mapcar (lambda (buffer)
                  (buffer-file-name buffer))
                (org-buffer-list 'files t))))
