;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Jason Kinsfather"
      user-mail-address "jasonrkinsfather@gmail")

(setq my/literate-config-file "~/.doom.d/config.org")

(setq my/org-directory "~/org/")

(setq my/work-directory (concat my/org-directory "work/")
      my/personal-directory (concat my/org-directory "personal/"))

(setq my/personal-capture-file (concat my/personal-directory "refile.org")
      my/work-capture-file (concat my/work-directory "refile.org"))

(setq my/main-contact-file (concat my/org-directory "contacts.org")
      my/contact-files (list my/main-contact-file))

(setq my/org-roam-directory "~/org-roam/")

(setq my/org-roam-dailies-directory "daily/")

(load-file (concat doom-private-dir "funcs.el"))

(setq doom-font (font-spec :family "Hack" :size 15)
      doom-variable-pitch-font (font-spec :family "Libre Baskerville")
      doom-serif-font (font-spec :family "Libre Baskerville"))

(when (file-exists-p "~/.doom.d/banners")
  (setq +doom-dashboard-banner-padding '(0 . 2)
        +doom-dashboard-banner-file "deepfield-window.png"
        +doom-dashboard-banner-dir "~/.doom.d/banners"))

(setq display-line-numbers-type t)

;; Thin grey line separating windows
(set-face-background 'vertical-border "grey")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

(setq doom-theme 'doom-nord-light)
(load-theme 'doom-nord-light t)

(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 200) ;chars
              (height . 82) ;lines
              (left . 50)
              (top . 0)))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 200)
              (height . 82)
              (left . 50)
              (top . 0))))
  (progn
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
    (setq default-frame-alist '( (tool-bar-lines . 0)))))



(setq org-directory my/org-directory)
(setq org-default-notes-file my/personal-capture-file)

(after! org
  (setq org-todo-keywords
    '((sequence "REPEAT(r)" "NEXT(n@/!)" "TODO(t@/!)" "WAIT(w@/!)" "SOMEDAY(s@/!)" "PROJ(p)" "|" "DONE(d@)" "CANCELLED(c@)")
      (sequence "GOAL(G)" "|" "ACHIEVED(a@)" "MISSED(m@)"))
        org-todo-keyword-faces
        '(("REPEAT" . (:foreground "orange" :weight 'bold))
          ("NEXT" . (:foreground "DarkOrange1" :weight 'bold))
          ("TODO" . (:foreground "blue" :weight 'normal))
          ("SOMEDAY" . (:foreground "sea green" :weight 'normal))
          ("WAIT" . (:foreground "dark yellow" :weight 'italic))
          ("PROJ" . (:foreground "hot pink" :weight 'normal))
          ("DONE" . (:foreground "light sea green" :weight 'normal))
          ("CANCELLED" . (:foreground "black" :weight 'normal))
          ("GOAL" . (:foreground "purple" :weight 'bold))
          ("ACHIEVED" . (:foreground "forest green" :weight 'bold))
          ("MISSED" . (:foreground "red" :weight 'italic)))))

(setq org-log-done 'time)

(setq org-log-into-drawer t)

(after! org-agenda
  (setq org-agenda-files (directory-files-recursively "~/org/personal/" "\\.org$")))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t
      org-agenda-include-diary t)

(use-package! org-super-agenda
    :commands (org-super-agenda-mode))

(after! org-agenda
  (org-super-agenda-mode))

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                           :todo "NEXT"
                           :order 1)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 6)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Assignments"
                           :tag "Assignment"
                           :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :order 12)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "Projects"
                           :tag "Project"
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :order 15)
                          (:name "To read"
                           :tag "Read"
                           :order 30)
                          (:name "Waiting"
                           :todo "WAITING"
                           :order 20)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY" )
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

(use-package! doct
  :commands (doct))

(after! org-capture
  (defun org-capture-select-template-prettier (&optional keys)
    "Select a capture template, in a prettier way than default Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
               '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
              (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                 "Template key: "
                 `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
  (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)
  
  (defun my/scroll (key &optional additional-keys)
      "Receive KEY and scroll the current window accordingly.
  When ADDITIONAL-KEYS is not nil, also include SPC and DEL in the
  allowed keys for scrolling, as expected in the export dispatch
  window."
    (let ((scrollup (if additional-keys '(?\s 22) 22))
          (scrolldown (if additional-keys `(?\d 134217846) 134217846)))
         (eval
          `(cl-case ,key
             ;; C-n
             (14
              (if (not (pos-visible-in-window-p (point-max)))
                  (ignore-errors (scroll-up 1))
                  (message "End of buffer")
                  (sit-for 1)))
             ;; C-p
             (16
              (if (not (pos-visible-in-window-p (point-min)))
                  (ignore-errors (scroll-down 1))
                  (message "Beginning of buffer")
                  (sit-for 1)))
             ;; SPC or
             (,scrollup
              (if (not (pos-visible-in-window-p (point-max)))
                  (scroll-up nil)
                  (message "End of buffer")
                  (sit-for 1)))
             ;; DEL
             (,scrolldown
              (if (not (pos-visible-in-window-p (point-min)))
                  (scroll-down nil)
                  (message "Beginning of buffer")
                  (sit-for 1)))))))
  
  (defun my/make-selection-read-key (allowed-keys prompt navigation-keys)
    "Read a key and ensure it is a member of ALLOWED-KEYS.
  Enable keys to scroll the window if NAVIGATION-KEYS are set.
  TAB, SPC, RET are treated equivalently."
    (setq header-line-format (when-navigation-keys "Use C-n, C-p, C-v, M-v to navigate."))
    (let ((char-key (read-char-exclusive prompt)))
         (if (and navigation-keys (memq char-key '(14 16 22 134217846)))
             (progn
               (my/scroll char-key)
               (my/make-selection-read-key allowed-keys prompt navigation-keys))
             (let ((key (char-to-string
                         (pcase char-key
                           ((or ?\s ?\t ?\r) ?\t)
                           (char char)))))
                  (if (member key allowed-keys)
                      key
                      (message "Invalid Key: `%s'" key)
                      (sit-for 1)
                      (my/make-selection-read-key allowed-keys prompt navigation-keys))))))
  
  (defun my/make-selection (table title &optional prompt specials)
      "Select a member of an alist with multiple keys. Prettified.
  
  TABLE is the alist which should contain entries where the car is a string.
  There should be two types of entries.
  
  1. prefix descriptions like (\"a\" \"Description\")
     This indicates that `a' is a prefix key for multi-letter selection, and
     that there are entries following with keys like \"ab\", \"ax\"…
  
  2. Select-able members must have more than two elements, with the first
     being the string of keys that lead to selecting it, and the second a
     short description string of the item.
  
  The command will then make a temporary buffer listing all entries
  that can be selected with a single key, and all the single key
  prefixes.  When you press the key for a single-letter entry, it is selected.
  When you press a prefix key, the commands (and maybe further prefixes)
  under this key will be shown and offered for selection.
  
  TITLE will be placed over the selection in the temporary buffer,
  PROMPT will be used when prompting for a key.  SPECIALS is an
  alist with (\"key\" \"description\") entries.  When one of these
  is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current-selector)
        (unwind-protect
            (catch 'exit
              ;; Start Selection Loop
              (while t
                (setq-local evil-normal-state-cursor (list nil))
                (erase-buffer)
                (insert title "\n\n")
                (let ((prefix-desc-keys nil)
                      (allowed-keys '("\C-g"))
                      (tab-alternatives '("\s" "\t" "\r"))
                      (cursor-type nil))
                  ;; Populate Allowed keys and Descriptions keys available with current selector.
                     (let ((re (format "\\`%s\\(.\\)\\'"
                                       (if current-selector
                                           (regexp-quote current-selector)
                                           "")))
                           (prefix (if current-selector
                                       (concat current-selector " ")
                                       "")))
                          (dolist (entry table)
                            (pcase entry
                              ;; Table Entry is a Description Key
                              (`(,(and key (pred (string-match re))) ,desc)
                                (let ((k (match-string 1 key)))
                                     (push k prefix-desc-keys)
                                     ;; Keys ending in tab, space or RET are equivalent.
                                     (if (member k tab-alternatives)
                                         (push "\t" allowed-keys)
                                         (push k allowed-keys))
                                     (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize ">" 'face 'font-lock-comment-face) "  " desc "..." "\n")))
                              ;; Table Entry is an Allowed Key
                              (`(,(and key (pred (string-match re))) ,desc . ,_)
                                (let ((k (match-string 1 key)))
                                     (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                                     (push k allowed-keys)))
                              ;;All other entries, do nothing.
                              (_ nil))))
                     ;; Insert Special Entries, if any.
                     (when specials
                       (insert "─────────────────────────\n")
                       (pcase-dolist (`(,key ,description) specials)
                         (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                         (push key allowed-keys)))
                     ;; Display UI and let user select an entry or a sub-level prefix.
                     ;; Begin by moving cursor to the character at the minimum accessible point in the selection buffer.
                     (goto-char (point-min)) ;
                     ;; If the maximum point of the buffer is not visible in the current window, resize window to fit the full buffer.
                     (unless (pos-visible-in-window-p (point-max))
                             (org-fit-window-to-buffer))
                     ;;
                     (let ((keypress (org--mks-read-key allowed-keys prompt t)))
                          (setq current-selector (concat current-selector keypress))
                          (cond
                            ;; User has requested aborting the selection.
                            ((equal keypress "\C-g") (user-error "Abort"))
                            ;; Selection is a prefix description:
                            ;; Continue Selection Loop, Opening a New Menu.
                            ((member keypress prefix-desc-keys))
                            ;; Selection matches an association:
                            ;; Exit the Selection Loop and return the alist entry for the current selector.
                            ((let ((entry (assoc current-selector table)))
                                  (and entry (throw 'exit entry))))
                            ;; Selection matches a special entry:
                            ;; Exit the Selection Loop and return the current selector.
                            ((assoc current-selector specials) (throw 'exit current-selector)
                            (t (error "No entry available")))))))
              (when buffer (kill-buffer buffer)))))))
  (advice-add 'org-mks :override #'my/make-selection)
  (setq +org-capture-recipes (concat (file-name-as-directory org-directory) "cook.org"))

  (defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
          (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
     (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
     (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                      (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

  (defun set-org-capture-templates ()
    (setq org-capture-templates
      (doct `(("Contact"
               :keys "c"
               :icon ("account_box" :set "material" :color "blue")
               :file my/main-contact-file
               :prepend t
               :template ("* %(org-contacts-template-name)"
                          ":PROPERTIES:"
                          ":ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}"
                          ":BIRTHDAY: %^{yyyy-mm-dd}"
                          ":EMAIL: %(org-contacts-template-email)"
                          ":NOTE: %^{NOTE}"
                          ":END:")
               :children (("Chosen Family"
                           :keys "c"
                           :icon ("favorite" :set "material"  :color "purple")
                           :headline "Chosen Family")
                          ("Blood Family"
                           :keys "b"
                           :icon ("invert_colors" :set "material" :color "red")
                           :headline "Blood Family")
                          ("Work"
                           :keys "w"
                           :icon ("work" :set "material" :color "brown")
                           :headline "Work")
                          ("Acquaintance"
                           :keys "a"
                           :icon ("pan_tool" :set "material" :color "green")
                           :headline "Acquaintance")))
              ("Personal Todo"
               :keys "t"
               :icon ("checklist" :set "octicon" :color "green")
               :file +org-capture-todo-file
               :prepend t
               :headline "Inbox"
               :type entry
               :template ("* TODO %?\n%U\n%a\n"))
              ("Personal Note"
               :keys "n"
               :icon ("sticky-note-o" :set "faicon" :color "green")
               :file +org-capture-todo-file
               :prepend t
               :headline "Inbox"
               :type entry
               :template ("* %?" "%i %a"))
              ("Email"
               :keys "e"
               :icon ("envelope" :set "faicon" :color "blue")
               :file +org-capture-todo-file
               :prepend t
               :headline "Inbox"
               :type entry
               :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                          "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                          "about %^{topic}"
                          "%U %i %a"))
               ("Interesting"
                :keys "i"
                :icon ("eye" :set "faicon" :color "lcyan")
                :file +org-capture-todo-file
                :prepend t
                :headline "Interesting"
                :type entry
                :template ("* [ ] %{desc}%? :%{i-type}:" "%i %a")
                :children (("Webpage"
                           :keys "w"
                           :icon ("globe" :set "faicon" :color "green")
                           :desc "%(org-cliplink-capture) "
                           :i-type "read:web")
                          ("Article"
                           :keys "a"
                           :icon ("file-text" :set "octicon" :color "yellow")
                           :desc ""
                           :i-type "read:research")
                          ("Cooking"
                           :keys "c"
                           :icon ("spoon" :set "faicon" :color "dorange")
                           :file +org-capture-recipes
                           :headline "Unsorted"
                           :template "%(org-chef-get-recipe-from-url)")
                          ("Information"
                           :keys "i"
                           :icon ("info-circle" :set "faicon" :color "blue")
                           :desc ""
                           :i-type "read:info")
                          ("Idea"
                           :keys "I"
                           :icon ("bubble_chart" :set "material" :color "silver")
                           :desc ""
                           :i-type "idea")))
               ("Tasks"
                :keys "k"
                :icon ("inbox" :set "octicon" :color "yellow")
                :file +org-capture-todo-file
                :prepend t
                :headline "Tasks"
                :type entry
                :template ("* TODO %? %^G%{extra}" "%i %a")
                :children (("General Tasks"
                            :keys "k"
                            :icon ("inbox" :set "octicon" :color "yellow")
                            :extra "")
                           ("Task with deadline"
                            :keys "d"
                            :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                            :extra "\nDEADLINE: %^{Deadline:}t")
                           ("Scheduled Task"
                            :keys "s"
                            :icon ("calendar" :set "octicon" :color "orange")
                            :extra "\nSCHEDULED: %^{Start time:}t")))
               ("Project"
                :keys "p"
                :icon ("repo" :set "octicon" :color "silver")
                :prepend t
                :type entry
                :headline "Inbox"
                :template ("* %{time-or-todo} %?" "%i" "%a")
                :file ""
                :custom (:time-or-todo "")
                :children (("Project-local todo"
                            :icon ("sticky-note" :set "faicon" :color "yellow")
                            :time-or-todo "%U"
                            :file +org-capture-project-notes-file)
                           ("Project-local changelog"
                            :keys "c"
                            :icon ("sticky-note" :set "faicon" :color"yellow")
                            :time-or-todo "%U"
                            :heading "Unreleased"
                            :file +org-capture-project-changelog-file)))
               ("\tCentralised project templates"
                :keys "o"
                :type entry
                :prepend t
                :template ("* %{time-or-todo} %?" "%i" "%a")
                :children (("Project todo"
                            :keys "t"
                            :prepend nil
                            :time-or-todo "TODO"
                            :heading "Tasks"
                            :file +org-capture-central-project-todo-file)
                           ("Project note"
                            :keys "n"
                            :time-or-todo "%U"
                            :heading "Notes"
                            :file +org-capture-central-project-notes-file)
                           ("Project changelog"
                            :keys "c"
                            :time-or-todo "%U"
                            :heading "Unreleased"
                            :file +org-capture-central-project-changelog-file)))))))

  (set-org-capture-templates)
  (unless (display-graphic-p)
    (add-hook 'server-after-make-frame-hook
              (defun org-capture-reinitialize-hook ()
                (when(display-graphic-p)
                  (set-org-capture-templates)
                  (remove-hook 'server-after-make-frame-hook
                               #'org-capture-reinitialize-hook
                               ))))))

(use-package! org-journal
  :after org
  :config
  (customize-set-variable 'org-journal-dir (concat org-roam-directory "journal"))
  (customize-set-variable 'org-journal-file-format "private-%Y-%m-%d.org")
  (customize-set-variable 'org-journal-date-prefix "#+TITLE: ")
  (customize-set-variable 'org-journal-time-prefix "* ")
  (customize-set-variable 'org-journal-time-format "")
  (customize-set-variable 'org-journal-carryover-items "TODO=\"TODO\"")
  (customize-set-variable 'org-journal-date-format "%Y-%m-%d")
  (map! :leader
        (:prefix-map ("n" . "notes")
          (:prefix ("j" . "journal")
            :desc "Today" "t" #'org-journal-today )))
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))

(require 'diary-lib)

(setq org-roam-directory my/org-roam-directory
      org-roam-dailies-directory my/org-roam-dailies-directory)

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (org-mode . org-roam-mode)
  :custom-face
  (org-roam-link ((t (:inherit org-link))))
  :init
  (require 'org-roam-protocol)
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-capture" "c" #'org-roam-capture ))

(setq org-roam-capture-templates
      '(("d" "default" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${title}\n"
         :unnarrowed t
         :immediate-finish t)))

;; (after! org-roam
;;   (setq my/org-roam-files (directory-files org-roam-directory  t ".*.org"))
;;   (setq my/org-roam-todo-file (concat org-roam-directory "todo.org"))
;;   (setq org-refile-targets `((,(append (my/open-org-files-list) (directory-files org-directory  t ".*.org")) :maxlevel . 7)))
;;   ;; (push my/org-roam-todo-file org-agenda-files)

;;   (defun my/org-roam-get-title (path)
;;     (save-window-excursion
;;       ;; A simple find-file didn't work when the original was narrowed
;;       (with-temp-buffer
;;         (insert-file-contents path)
;;         (org-mode)
;;         (car (org-roam--extract-titles-title)))))

;;   (add-to-list 'org-capture-templates '("r" "org-roam todo" entry (file my/org-roam-todo-file)
;;                                         "* TODO %?  #[[%F][%(my/org-roam-get-title \"%F\")]]\n%i\n%a")))

(after! org-roam
  (map! :leader
        :prefix ("m" . "Roam Dailies")
        (:prefix ("f" . "Find Daily File")
          :desc "Find Today's Daily" "t" #'org-roam-dailies-find-today
          :desc "Find Yesterday's Daily" "y" #'org-roam-dailies-find-yesterday
          :desc "Find Daily on Date" "d" #'org-roam-dailies-find-date )
        (:prefix ("c" . "Capture Daily File")
          :desc "Capture Today's Daily" "t" #'org-roam-dailies-capture-today
          :desc "Capture Yesterday's Daily" "y" #'org-roam-dailies-capture-yesterday
          :desc "Capture Daily on Date" "d" #'org-roam-dailies-capture-date )))

(setq org-roam-dailies-capture-templates
      '(("j" "journal" entry
        #'org-roam-capture--get-point
        "* %?"
        :file-name "daily/%<%Y-%m-%d>"
        :head "#+title: %<%Y-%m-%d>\n"
        :olp ("My Journal"))))

(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts my/contact-files))

(use-package! toc-org
  :hook (org-mode . toc-org-mode))

(use-package! org-download
  :config
  ;; take an image that is already on the clipboard
  (customize-set-variable 'org-download-screenshot-method "xclip -selection clipboard -t image/png -o > %s"))

(use-package! org-cliplink)

(use-package! aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (common-lisp-mode . aggressive-indent-mode))

(use-package! multiple-cursors
              :init
              (setq mc/always-run-for-all t)
              :config
              (add-to-list 'mc/unsupported-minor-modes 'lispy-mode)
              :bind (("C-S-c" . mc/edit-lines)
                     ("C-M-g" . mc/mark-all-like-this-dwim)
                     ("C->" . mc/mark-next-like-this)
                     ("C-<" . mc/mark-previous-like-this)
                     ("C-)" . mc/skip-to-next-like-this)
                     ("C-M->" . mc/skip-to-next-like-this)
                     ("C-(" . mc/skip-to-previous-like-this)
                     ("C-M-<" . mc/skip-to-previous-like-this)))

(after! ivy
  ;; Causes open buffers and recentf to be combined in ivy-switch-buffer
  (setq ivy-use-virtual-buffers t
        counsel-find-file-at-point t
        ivy-wrap nil
        ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-height-alist '((t . 20))
        ivy-posframe-parameters '((internal-border-width . 1))
        ivy-posframe-width 100)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map (kbd "M-r") 'counsel-esh-history))))

(after! dired
  (setq dired-listing-switches "-aBhl  --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies (quote always)
        dired-recursive-deletes (quote top)
        ;; Directly edit permisison bits!
        wdired-allow-to-change-permissions t
        dired-omit-mode nil))

(use-package! dired-narrow
              :commands (dired-narrow-fuzzy)
              :init
              (map! :map dired-mode-map
                    :desc "narrow" "/" #'dired-narrow-fuzzy))

;; Directly edit permisison bits!
(setq wdired-allow-to-change-permissions t)

(use-package! magit
  :config
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-unstage-all-confirm nil)

  (remove-hook 'magit-mode-hook 'turn-on-magit-gitflow)

  ;; Restores "normal" behavior in branch view (when hitting RET)
  (setq magit-visit-ref-behavior '(create-branch checkout-any focus-on-ref))

  (setq git-commit-finish-query-functions nil)
  (setq magit-visit-ref-create 1)
  (setq magit-revision-show-gravatars nil))

(after! (magit key-chord)
  (add-to-sl-keymap
   '(("k" . magit-dispatch-popup)
     ("s" . magit-status)
     ("o" . magit-log)
     ("u" . magit-submodule-update)
     ("l" . magit-show-refs-head))))

(use-package! real-auto-save
  :hook
  (prog-mode . real-auto-save-mode)
  (org-mode . real-auto-save-mode))



(flycheck-mode 0)

(setq direnv-always-show-summary nil)

(add-to-list 'auto-mode-alist '("\\.eps\\'" . doc-view-minor-mode))

;; all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Coordinate between kill ring and system clipboard
(setq save-interprogram-paste-before-kill t)

(setq eshell-history-file-name (concat doom-private-dir "eshell-history"))

;; This is dangerous, but reduces the annoying step of confirming local variable settings each time
;; a file with a "Local Variables" clause (like many Org files) is opened.
(setq enable-local-variables :all)

;; This is usually just annoying
(setq compilation-ask-about-save nil)

;; No confirm on exit
(setq confirm-kill-emacs nil)


;; Help out Projectile for remote files via TRAMP
;; https://sideshowcoder.com/2017/10/24/projectile-and-tramp/
(defadvice projectile-on (around exlude-tramp activate)
  "This should disable projectile when visiting a remote file"
  (unless  (--any? (and it (file-remote-p it))
                   (list
                    (buffer-file-name)
                    list-buffers-directory
                    default-directory
                    dired-directory))
    ad-do-it))

(setq projectile-mode-line "Projectile")

(setq password-store-password-length 20)

;; Truncate compiilation buffers, otherwise Emacs gets slow
;; https://stackoverflow.com/questions/11239201/can-i-limit-the-length-of-the-compilation-buffer-in-emacs
(add-hook 'compilation-filter-hook 'comint-truncate-buffer)
(setq comint-buffer-maximum-size 2000)

(setq recentf-max-saved-items 10000)
