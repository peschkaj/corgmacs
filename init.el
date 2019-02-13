;;; -*- lexical-binding: t -*-
;;;
;;; init.el - where the corgs begin
;;; corgmacs --- Summary
;;;
;;; Being the emacs configuration of Jeremiah Peschka

;; Copyright (C) 2019 Jeremiah Peschka <jeremiah@legit.biz>
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
;;; Please don't provide any, this is garbage.

;;; Code:
;;; Code goes here, moron.

;; Set up list of package repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; avoid problems with files newer than their byte-compiled
;; counterparts it's better to have a slower startup than load an
;; outdated and maybe bugged package
(setq load-prefer-newer t)
;; initialize the packages and create the packages list if not exists
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; benchmark-init
;;
;; Uncomment this if emacs is being slow on startup.
;; View important timing stats with M-x bencmark-init/show-durations-tabluation
;;
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(when (not package-archive-contents)
  (package-refresh-contents))

;; install use-package if not exists
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless (package-installed-p 'which-key)
  (package-install 'which-key))

(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(unless (package-installed-p 'smartparens)
  (package-install 'smartparens))

(unless (package-installed-p 'paredit)
  (package-install 'paredit))

(unless (package-installed-p 'rainbow-delimiters)
  (package-install 'rainbow-delimiters))

(unless (package-installed-p 'hydra)
  (package-install 'hydra))

(unless (package-installed-p 'restart-emacs)
  (package-install 'restart-emacs))

(unless (package-installed-p 'helm)
  (package-install 'helm))

(unless (package-installed-p 'dash-functional)
  (package-install 'dash-functional))

(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

(unless (package-installed-p 'racket-mode)
  (package-install 'racket-mode))

(eval-when-compile
  (require 'use-package)
  (require 'ibuffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up loading from modules directory
;;
;; This code is borrowed from Bodil Stokke's excell ohai-emacs
;; see https://github.com/bodil/ohai-emacs for the original.
;;
;; Figure out the path to our .emacs.d by getting the path part of the
;; current file (`init.el`).
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) (file-chase-links load-file-name))))

;; Individual modules are stored in a `modules` subdirectory of .emacs.d; it's
;; necessary to add these modules explicitly to the load-path.
(add-to-list 'load-path (concat dotfiles-dir "modules"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp settings
;;
;; ZSH on the PDX servers caused problems with tramp hanging.
;; Force to bash instead to make life simple.
;;
;; Mon Jun 19 17:43:56 PDT 2017
;; I'm not sure but this _might_ be causing problems for my local TRAMP mode
;;
;; Wed Dec 26 07:45:35 PDT 2018
;; tramp hangs are fixed by adding the following AT THE VERY END of .zshrc
;; (or .zshrc.local)
;; [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
(use-package tramp
  :init (setq tramp-ssh-controlmaster-options nil))

(require 'corgmacs-general)
(require 'corgmacs-editing)
(require 'corgmacs-helm)
(require 'corgmacs-vc)
(require 'corgmacs-code)
(require 'corgmacs-lisp)
(require 'corgmacs-cpp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell
(require 'corgmacs-haskell)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up base folders for documents and PDFs and such...
(defvar jp/docs (expand-file-name "~/Documents/") "Documents folder.")
(defvar jp/papers-base (concat jp/docs "reading/") "Location for reading library including PDFs, bibliography, and notes.")
(defvar jp/papers-pdfs (concat jp/papers-base "lib/") "PDF folder.")
(defvar jp/papers-notes (concat jp/papers-base "index.org") "Default location for notes about papers.")
(defvar jp/papers-refs  (concat jp/papers-base "index.bib") "Bibliography.")
(setq bibtex-completion-bibliography (list jp/papers-refs)
      bibtex-completion-library-path jp/papers-pdfs
      bibtex-completion-notes-path   jp/papers-notes
      bibtex-completion-pdf-field    "file"
      bibtex-completion-display-formats '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${author:36} ${title:*} ${journal:40}")
                                          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${author:36} ${title:*} Chapter ${chapter:32}")
                                          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${author:36} ${title:*} ${booktitle:40}")
                                          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${author:36} ${title:*} ${booktitle:40}")
                                          (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3}  ${author:36} ${title:*}"))
      bibtex-completion-additional-search-fields '(keywords journal title)
      reftex-default-bibliography  (list jp/papers-refs)
      org-ref-completion-libary   'org-ref-helm-cite
      org-ref-notes-directory      jp/papers-base
      org-ref-bibliography-notes   jp/papers-notes
      org-ref-default-bibliography (list jp/papers-refs)
      org-ref-pdf-directory        jp/papers-pdfs)

(require 'corgmacs-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font ligatures
;;
;; TODO Replace this with a check that either loads mac-auto-operator-composition-mode
;;      -OR- uses prettify symbols
(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

(set-default-font "PragmataPro Liga 16" t t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macOS:
;;
;; Raise emacs on activation
(when (featurep 'ns)
  (defun ns-raise-emacs ()
    "Raise Emacs."
    (ns-do-applescript "tell application \"Emacs\" to activate"))

  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided frame."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))

  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)

  (when (display-graphic-p)
    (ns-raise-emacs)))

;; Set right command to super
(setq mac-right-command-modifier 'super)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pretty symbols
(setq prettify-symbols-unprettify-at-point 'right-edge)

(defconst pragmatapro-prettify-symbols-alist
  (mapcar (lambda (s)
            `(,(car s)
              .
              ,(vconcat
                (apply 'vconcat (make-list (- (length (car s)) 1) (vector (decode-char 'ucs #X0020) '(Br . Bl))))
                (vector (decode-char 'ucs (cadr s))))))
          '(("[ERROR]"   #XE380)
            ("[DEBUG]"   #XE381)
            ("[INFO]"    #XE382)
            ("[WARN]"    #XE383)
            ("[WARNING]" #XE384)
            ("[ERR]"     #XE385)
            ("[FATAL]"   #XE386)
            ("[TRACE]"   #XE387)
            ("[FIXME]"   #XE388)
            ("[TODO]"    #XE389)
            ("TODO"      #XE389)
            ("[BUG]"     #XE38A)
            ("[NOTE]"    #XE38B)
            ("[HACK]"    #XE38C)
            ("[MARK]"    #XE38D)
            (":/"        #XE9B9)
            (":\\"       #XE9BA)
            (":3"        #XE9BB)
            (":D"        #XE9BC)
            (":P"        #XE9BD)
            (":>:"       #XE9BE)
            (":<:"       #XE9BF)
            ("<\\>"      #XE9DD)
            (">-"        #XEA20)
            (">="        #XEA21)
            (">>"        #XEA22)
            (">>-"       #XEA23)
            (">>="       #XEA24)
            (">>>"       #XEA25)
            (">=>"       #XEA26)
            (">>^"       #XEA27)
            ("??"        #XEA40)
            ("?~"        #XEA41)
            ("?="        #XEA42)
            ("?>"        #XEA43)
            ("???"       #XEA44)
            ("^="        #XEA48)
            ("^."        #XEA49)
            ("^?"        #XEA4A)
            ("^.."       #XEA4B)
            ("^<<"       #XEA4C)
            ("^>>"       #XEA4D)
            ("^>"        #XEA4E)
            ("\\\\"      #XEA50)
            ("\\>"       #XEA51)
            ("\\/-"      #XEA52))))

(defun fp-prettify-symbols ()
  "Make the letters pretty!"
  (mapc (lambda (pair) (push pair prettify-symbols-alist))
        ;; Need a way to make this work with comments as well.
        '(
          ("\\" . ?λ)
          ("*" . ?⋅)
          ;; ("forall" . ?∀)
          ;; ("forAll" . ?∀)
          ;; ("all"    . ?∀)
          ;; ("exists" . ?∃)
          ;; ("undefined" . ?⊥)
          ;; ("elem" . ?∈)
          ;; ("flip elem" . ?∋)
          ;; ("notElem" . ?∉)
          ;; ("flip notElem" . ?∌)
          ;; ("member" . ?∈)
          ;; ("notMember" . ?∉)
          ;; ("union" . ?⋃)
          ;; ("intersection" . ?⋂)
          ;; ("isSubsetOf" . ?⊆)
          ;; ("isProperSubsetOf" . ?⊂)
          (" . " . (? (Br . Bl) ?◦ (Br . Bl) ? ))
          ;; ("/" . ?÷)
          ;; ("div" . ?÷)
          ;; ("quot" . ?÷)
          )))


(defun add-pragmatapro-prettify-symbols-alist ()
  "Make sure the letters are pretty when we use Pragmata Pro."
  (dolist (alias pragmatapro-prettify-symbols-alist)
    (push alias prettify-symbols-alist)))


(when (display-graphic-p)
  (add-hook 'haskell-mode-hook #'fp-prettify-symbols)
  (add-hook 'prog-mode-hook #'add-pragmatapro-prettify-symbols-alist)
  (toggle-frame-maximized)
  (global-prettify-symbols-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown
(use-package markdown-mode
  :ensure t
  :config (autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable smooth scrolling
(when (eq system-type 'darwin)
  (if mac-mouse-wheel-smooth-scroll
      (setq mac-mouse-wheel-smooth-scroll nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyfreq
;;
;; tracks how often commands are used without a shortcut
;; view usage with keyfreq-show
(use-package keyfreq
  :ensure t
  :blackout t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; notmuch configuration
;; (autoload 'notmuch "notmuch" "notmuch mail" t)

;; (setq mail-user-agent 'message-user-agent
;;       user-mail-address "jpeschka@pdx.edu"
;;       user-full-name "Jeremiah Peschka"
;;       smtpmail-stream-type 'ssl
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 465
;;       smtpmail-debug-info t
;;       message-send-mail-function 'message-smtpmail-send-it
;;       message-default-mail-headers "Cc: \nBcc: \n"
;;       message-auto-save-directory "~/.mail/pdx/draft"
;;       message-kill-buffer-on-exit t
;;       message-directory "~/.mail")

;; (defun notmuch-exec-offlineimap ()
;;     "execute offlineimap"
;;     (interactive)
;;     (set-process-sentinel
;;      (start-process-shell-command "offlineimap"
;;                                   "*offlineimap*"
;;                                   "offlineimap -o")
;;      '(lambda (process event)
;;         (notmuch-refresh-all-buffers)
;;         (let ((w (get-buffer-window "*offlineimap*")))
;;           (when w
;;             (with-selected-window w (recenter (window-end)))))))
;; ;    (popwin:display-buffer "*offlineimap*")
;;     )

;; (add-to-list 'popwin:special-display-config
;;              '("*offlineimap*" :dedicated t :position bottom :stick t
;;                :height 0.4 :noselect t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configuration for spaceline + spaceline-all-the-icons
(use-package all-the-icons
  :ensure t)

(use-package nyan-mode
  :ensure t
  :config (nyan-mode))

(use-package git-gutter
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'git-gutter-mode))

(use-package fancy-battery
  :ensure t
  :config (fancy-battery-mode))

;; Both spaceline and spaceline-all-the-icons are pinned to melpa-stable
;; this is because of this bug https://github.com/domtronn/spaceline-all-the-icons.el/issues/100
;; and the fix was found via https://github.com/flamingbear/emacs-config/commit/6594677de383d742acaccc293a260b9adff7e4cc
(use-package spaceline
  :ensure t
  :pin melpa-stable)

(use-package spaceline-config
  :ensure spaceline
  :after spaceline
  :config
  (setq powerline-default-separator 'bar))

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :pin melpa-stable
  :config

  (setq spaceline-all-the-icons-separator-type 'none)
  (setq spaceline-all-the-icons-icon-set-modified 'circle)

  (spaceline-toggle-all-the-icons-vc-icon-on)
  (spaceline-toggle-all-the-icons-mode-icon-on)
  (spaceline-toggle-all-the-icons-vc-status-on)
  (spaceline-toggle-all-the-icons-text-scale-on)
  ;; p sure that the time is leading to more power draw
  (spaceline-toggle-all-the-icons-time-off)
  ;; git-ahead is currently disabled because it starts spamming messages that
  ;; 'Buffer " *temp*" has a running processes' but nobody actually cares.
  ;; (spaceline-all-the-icons--setup-git-ahead)
  ;; (spaceline-toggle-all-the-icons-git-ahead-on)

  (spaceline-toggle-all-the-icons-git-status-on)
  (spaceline-all-the-icons-theme))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-term
(use-package multi-term
  :ensure t
  :config
  (add-hook 'term-mode-hook (lambda ()
                              (define-key term-raw-map (kbd "C-y") 'term-paste)))
  (setq multi-term-program "/usr/local/bin/zsh"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; treemacs
(use-package treemacs
  :ensure t
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)


(use-package langtool
  :ensure t
  :config
  (progn (setq langtool-language-tool-server-jar "/usr/local/Cellar/languagetool/4.4/libexec/languagetool.jar"
               langtool-server-user-arguments '("-p" "8082")
               langtool-bin "/usr/local/bin/languagetool")
         ;; TODO move these to :bind
         (global-set-key "\C-x4w" 'langtool-check)
         (global-set-key "\C-x4W" 'langtool-check-done)
         (global-set-key "\C-x4l" 'langtool-switch-default-language)
         (global-set-key "\C-x44" 'langtool-show-message-at-point)
         (global-set-key "\C-x4c" 'langtool-correct-buffer)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load theme as the very last activity
(load-theme 'tangotango t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global blackouts
(blackout 'auto-revert-mode)


(server-start)
(when (string-equal system-type "darwin")
  (mac-pseudo-daemon-mode)
  ;; (defun jp/save-buffers-kill-emacs ()
  ;;   (interactive)
  ;;   (if (not (eq mac-pseudo-daemon-mode 'nil))
  ;;       (mac-pseudo-daemon-mode))
  ;;   (save-buffers-kill-emacs))
  ;; (global-unset-key (kbd "C-x C-c"))
  ;; (global-set-key (kbd "C-x C-c") 'jp/save-buffers-kill-emacs)
  )



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
