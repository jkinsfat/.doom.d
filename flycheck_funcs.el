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

(defcustom org-agenda-foci nil
  "Foci are sets of files that org-agenda can focus on, omitting entries from all other agenda files."
  :group 'org-agenda
  :type `(repeat
          (choice (list :tag "Foci Entry"
                        (string :tag "Selector   ")
                        (string :tag "Description")
                        (repeat (file :tag "Foci File")))
                  (list :tag "Multikey Description"
                        (string :tag "Selector   ")
                        (string :tag "Description")))))

(setq org-agenda-foci '(("a" "All Agenda Files" ("~/org/personal/finance.org"))))

(defun my/org-agenda-select-foci (&optional selector)
  "Select a foci for Org Agenda.
   Can force a foci, avoiding selection buffer, by setting SELECTOR to a string."
  (let ((org-agenda-foci (or (org-agenda-foci)
                             '(("a" "All" org-agenda-files))))
       (if selector
           (or (assoc selector org-agenda-foci)
               (error "No agenda foci referred to by \"%s\" keys" selector))
           (my/make-selection org-agenda-foci
                              "Select an Agenda Foci\n==================="
                              "Foci Key: "
                              '(("q" "Abort")))))))
