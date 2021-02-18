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

;; (defun my/capture-string (string &optional keys)
;;   (interactive "sInitial text: \n")
;;   (let ((capture-initial string)
;;         (capture-entry (capture-select-template keys)))
;;     (org-capture)))

(defcustom org-agenda-foci nil
  "A focus is sets of files that org-agenda can load entries from, omitting entries from all other agenda files."
  :group 'org-agenda
  :type `(repeat
          (choice (list :tag "Focus Entry"
                        (string :tag "Selector   ")
                        (string :tag "Description")
                        (repeat (file :tag "Agenda File")))
                  (list :tag "Focus Prefix"
                        (string :tag "Selector Prefix   ")
                        (string :tag "Prefix Description")))))

(setq org-agenda-foci `(("a" "All Life Domains" ,(append
                                                  (directory-files-recursively my/personal-directory "\\.org$")
                                                  (directory-files-recursively my/work-directory "\\.org$")))
                        ("p" "Personal Life" ,(directory-files-recursively my/personal-directory "\\.org$"))
                        ("w" "Work Life" ,(directory-files-recursively my/work-directory "\\.org$"))))

(defun my/org-agenda-select-focus (&optional selector)
  "Select a foci for Org Agenda.
   Can force a foci, avoiding selection buffer, by setting SELECTOR to a string."
  (let ((foci (or org-agenda-foci
                  '(("a" "All" org-agenda-files)))))
       (if selector
           (or (assoc selector foci)
               (error "No agenda foci referred to by \"%s\" keys" selector))
           (my/make-selection foci
                              "Select an Agenda Focus\n==================="
                              "Focus Key: "
                              '(("C" "Customize")
                                ("R" "Restore Default")
                                ("q" "Abort"))))))

(defun my/org-agenda-focus-get-files (focus)
  (nth 2 focus))

(defun my/org-agenda-focus (&optional selector)
  (interactive "P")
  (when (get-buffer org-agenda-buffer-name) (org-agenda-exit))
  (setq org-agenda-default-focus org-agenda-files)
  (let ((original-buffer (buffer-name))
        (selection (my/org-agenda-select-focus))
        focus)
       (cond
        ((equal selection "C")
         (customize-variable 'org-agenda-foci))
        ((equal selection "q")
         (user-error "Abort"))
        ((equal selection "R")
         (setq org-agenda-files org-agenda-default-focus))
        (t
         (setq org-agenda-files (my/org-agenda-focus-get-files selection))
         (org-agenda)))))
