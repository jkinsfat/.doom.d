;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! real-auto-save)
(package! aggressive-indent)
(package! org-super-agenda :pin "f5e80e4d0d...")
(package! doct                  ;; Simpler Org Capture definitions
  :recipe (:host github :repo "progfolio/doct")
  :pin "a795fa4eaf...")
(package! toc-org)              ;;Update Org file table of contents without exporting
