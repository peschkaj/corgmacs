;;; -*- lexical-binding: t -*-
;;; corgmacs-org.el

;; Copyright (c) 2019 Jeremiah Peschka <jeremiah@legit.biz>
;;
;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;; org-mode corg-mode best-mode

;;; Code:
;;; Code goes here, moron.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up base folders for documents and PDFs and such...
;;
;; The assumed hierarchy is:
;;   Documents
;;   |- reading
;;   |  |- lib
;;   |
;;   |- index.org
;;   |- index.bib
(defvar corgmacs/docs
  (expand-file-name "~/Documents/") "Documents folder.")
(defvar corgmacs/papers-base
  (concat corgmacs/docs "reading/") "Location for reading library including PDFs, bibliography, and notes.")
(defvar corgmacs/papers-pdfs
  (concat corgmacs/papers-base "lib/") "PDF folder.")
(defvar corgmacs/papers-notes
  (concat corgmacs/papers-base "index.org") "Default location for notes about papers.")
(defvar corgmacs/papers-refs
  (concat corgmacs/papers-base "index.bib") "Bibliography.")
(setq bibtex-completion-bibliography (list corgmacs/papers-refs)
  bibtex-completion-library-path corgmacs/papers-pdfs
  bibtex-completion-notes-path   corgmacs/papers-notes
  bibtex-completion-pdf-field    "file"
  bibtex-completion-display-formats
  '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${author:36} ${title:*} ${journal:40}")
    (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${author:36} ${title:*} Chapter ${chapter:32}")
    (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${author:36} ${title:*} ${booktitle:40}")
    (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${author:36} ${title:*} ${booktitle:40}")
    (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${author:36} ${title:*}"))
  bibtex-completion-additional-search-fields '(keywords journal title)
  reftex-default-bibliography  (list corgmacs/papers-refs)
  org-ref-completion-libary   'org-ref-helm-cite
  org-ref-notes-directory      corgmacs/papers-base
  org-ref-bibliography-notes   corgmacs/papers-notes
  org-ref-default-bibliography (list corgmacs/papers-refs)
  org-ref-pdf-directory        corgmacs/papers-pdfs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
(use-package org
  ;:defer 1
  :ensure org-plus-contrib
  :config
  ;; Set the top-level org directory
  (setq org-directory (expand-file-name "~/org/"))
  ;; Auto save org files
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  ;; If a custom set of agenda files is supplied, use them.
  (if (fboundp 'corgmacs/set-org-agenda-files)
      (corgmacs/set-org-agenda-files))
  (setq split-height-threshold nil
        org-export-with-smart-quotes t)
  (progn
    ;; save org
    (advice-add 'org-refile :after 'org-save-all-org-buffers)
    ;; Provide advise on screen splitting
    (defadvice org-agenda (around split-vertically activate)
      (let ((split-width-threshold 80)) ; or whatever width makes sense for you
        ad-do-it))
    ;; set the modules enabled by default
    (setq org-modules '(org-bbdb
                        org-bibtex
                        org-docview
                        org-mhe
                        org-rmail
                        org-crypt
                        org-protocol
                        org-gnus
                        org-id
                        org-info
                        org-habit
                        org-irc
                        org-annotate-file
                        org-eval
                        org-expiry
                        org-man
                        org-panel
                        org-toc)
          fill-column 80)

    ;; set default directories
    (setq corgmacs/org-inbox-file (concat org-directory "inbox.org"))
    (setq org-default-notes-file corgmacs/org-inbox-file)

    ;; set the archive
    (setq org-agenda-custom-commands
          '(("Q" . "Custom queries") ;; gives label to "Q"
            ("Qa" "Archive search" search ""
             ((org-agenda-files (file-expand-wildcards (concat org-directory "*.org_archive")))))
            ;; ...other commands here
            ))
    (define-key global-map (kbd "C-c a") 'org-agenda)
    (define-key global-map (kbd "C-c c") 'nil)
    (define-key global-map (kbd "C-c c") 'org-capture)

    ;; highlight code blocks syntax
    (setq org-src-fontify-natively  t
          org-src-tab-acts-natively t)
    (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

    ;; more sane emphasis regex to export to HTML as substitute of Markdown
    (org-set-emph-re 'org-emphasis-regexp-components
                     '(" \t({"
                       "- \t.,:!?;)}[:multibyte:]"
                       " \t\r\n,"
                       "."
                       1))

    ;; highlight code blocks syntax in PDF export
    ;; Include the latex-exporter
    (use-package ox-latex)
    (add-to-list 'org-latex-classes
             '("koma-article"
               "\\documentclass{scrartcl}
                \\usepackage{microtype}
                \\usepackage{tgtermes}
                \\usepackage[scale=.9]{tgheros}
                \\usepackage{tgcursor}
                \\usepackage{paralist}
                \\newcommand{\\rc}{$^{14}C$}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    ;; Add minted to the defaults packages to include when exporting.
;    (add-to-list 'org-latex-packages-alist '("" "minted"))
;    (add-to-list 'org-latex-packages-alist '("" "xunicode"))
    ;; Tell the latex export to use the minted package for source
    ;; code coloration.
    ;; (setq org-latex-listings 'minted)
    ;; Let the exporter use the -shell-escape option to let latex
    ;; execute external programs.
    ;; This obviously and can be dangerous to activate!
    (setq org-latex-pdf-process
          '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

    ;; tasks management
    (setq org-log-done t)
    (setq org-clock-idle-time nil)
    (setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)"))
          org-habit-graphs-everywhere t                                            ;; Configuring display of org-habit in the agenda buffer
          org-habit-graph-column 80
          org-habit-preceding-days 14
          org-habit-show-habits-only-for-today nil
          org-src-tab-acts-natively t
          org-reverse-note-order t
          ;; diary is a 0 length file to keep emacs happy
          diary-file "~/Documents/org/diary"

          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-if-done t
          org-agenda-ndays 7
          org-agenda-show-all-dates t
          org-agenda-use-time-grid t
          org-deadline-warning-days 14
          org-journal-dir (concat org-directory "journal/")
          org-journal-file-format "%Y%m%d"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          ;; The next two lines set new journal entries to be top level
          ;; headlines and then tell org-journal to not insert the time
          ;; as a headline
          org-journal-time-prefix "* "
          org-journal-time-format ""
          ;; Include agenda archive files when searching
          org-agenda-text-search-extra-files (quote (agenda-archives))
          ;; sets org-refile to be able to work with any org-agenda-files
          org-refile-targets (quote ((nil :maxlevel . 9)
                                     (org-agenda-files :maxlevel . 9))))

    ;; Start on the current day, not Monday.
    (setq org-agenda-start-on-weekday nil)

    ;; Set up notifications for org
    (require 'appt)
    (setq appt-time-msg-list nil          ;; clear existing appt list
          appt-display-interval '10       ;; warn every 10 minutes from t - appt-message-warning-time
          appt-message-warning-time '10   ;; sent first warning 10 minutes before appointment
          appt-display-mode-line nil      ;; don't show in the modeline
          appt-display-format 'window)    ;; passes notifications to the designated window function-key-map
    (appt-activate 1)                     ;; activate appointment notification
    ;(display-time)                        ;; activate time display

    (org-agenda-to-appt)                  ;; generate the appt list from org agenda files on emacs launch
    (run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
    (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view


    ;; designate the window function for my-appt-send-notification
    (defun corgmacs/appt-display (min-to-app new-time msg)
      (corgmacs/appt-send-notification
       (format "'Appointment in %s minutes'" min-to-app)    ;; passed to -title in terminal-notifier call
       (format "'%s'" msg)))                                ;; passed to -message in terminal-notifier call
    (setq appt-disp-window-function (function corgmacs/appt-display))

    ;; t - Prompt for a title and then add to index.org unless you refile it
    (setq org-capture-templates
          '(("t" "todo" entry (file corgmacs/org-inbox-file)
             "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
            ("m" "Meeting" entry (file org-default-notes-file)
             "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
            ("i" "Idea" entry (file corgmacs/org-inbox-file)
             "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
            ("n" "Next Task" entry (file+headline org-inbox-file "Tasks")
             "** NEXT %? \nDEADLINE: %t")))


    ;; (defun update-org-src-locs ()
    ;;   (when (string= major-mode "org-mode")
    ;;     (save-excursion
    ;;       (org-element-map (org-element-parse-buffer) 'headline
    ;;         (lambda (hl)
    ;;           (goto-char (org-element-property :begin hl))
    ;;           (forward-line -1)
    ;;           (when (string= (buffer-substring-no-properties (point) (line-end-position))
    ;;                          "#+END_SRC")
    ;;             (forward-line)
    ;;             (insert "\n")))))))

    ;; (add-hook 'after-save-hook 'update-org-src-locs)

    (defun corgmacs/org-template ()
      (insert "#+AUTHOR: Jeremiah Peschka
#+EMAIL: jeremiah.peschka@gmail.com
#+STARTUP: indent showall
#+OPTIONS: tags:nil")
      (org-mode-restart))

    (define-auto-insert "\\.org$" #'corgmacs/org-template)


    ;; agenda & diary
    (setq org-agenda-include-diary nil)
    (setq org-agenda-inhibit-startup t)


    ;; protect hidden trees for being inadvertily edited (do not work with evil)
    (setq-default org-catch-invisible-edits  'error
                  org-ctrl-k-protect-subtree 'error)

    ;; show images inline
    ;; only works in GUI, but is a nice feature to have
    (when (window-system)
      (setq org-startup-with-inline-images t))
    ;; limit images width
    (setq org-image-actual-width '(800))))

(use-package org-ref
  :after org
  :ensure t
  :config
  ;; (setq org-latex-pdf-process
  ;;     '("pdflatex -interaction nonstopmode -output-directory %o %f"
  ;;       "bibtex %b"
  ;;       "pdflatex -interaction nonstopmode -output-directory %o %f"
  ;;       "pdflatex -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))
(use-package doi-utils
  :after org-ref)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOAR ORG
;;
;; enable auto-fill-mode for org
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;; org-journal gives C-c C-j to create a new journal entry
(use-package org-journal
  :ensure t
  :config
  (define-key global-map (kbd "C-c j") 'org-journal-new-entry))
;; pretty bullets
(use-package org-bullets
  :ensure t
  :config
  ;; always pretty bullets
  (add-hook 'org-mode-hook
            (lambda
              ()
              (org-bullets-mode 1))))


(provide 'corgmacs-org)
;;; corgmacs-org.el ends here
