;;; corgmacs --- Summary
;;;
;;; Being the emacs configuration of Jeremiah Peschka
;;;
;;; Commentary:
;;; Please don't provide any, this is garbage.
;;;
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

(unless (package-installed-p 'rainbow-delimiters)
  (package-install 'rainbow-delimiters))

(unless (package-installed-p 'hydra)
  (package-install 'hydra))

(unless (package-installed-p 'restart-emacs)
  (package-install 'restart-emacs))

(unless (package-installed-p 'helm)
  (package-install 'helm))

(eval-when-compile
  (require 'use-package)
  (require 'ibuffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blackout - control the mode line
(add-to-list 'load-path "~/src/blackout")
(use-package blackout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key allows the lazy emacs user to start typing a key combination,
;; pause, and then see what's actually possible
(use-package which-key
  :blackout t)
(which-key-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yay for custom menus via hydra
(use-package hydra
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pretty parens and curly bois
(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook
          #'rainbow-delimiters-mode)
;; smartparens cheatsheet can be found at
;; https://gist.github.com/pvik/8eb5755cc34da0226e3fc23a320a3c95
(require 'smartparens-config)

(add-hook 'prog-mode-hook
          #'smartparens-strict-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; restart emacs
;;
;; Provides a way to quit or restart emacs, bound to C-x r r
(use-package restart-emacs
  :ensure t)
(define-key global-map (kbd "C-x r r") 'restart-emacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
(require 'helm)
(use-package helm-bibtex
  :ensure t)
;; enable fuzzy match
(setq helm-mode-fuzzy-match                 t
      helm-completion-in-region-fuzzy-match t
      helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-bookmarks
                                  helm-source-buffer-not-found))
;; some keybinds
(global-set-key (kbd "M-x")                          'nil)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "C-x b")                        'nil)
(global-set-key (kbd "C-x b")                        'helm-mini)
(global-set-key (kbd "\C-cfr")                       'helm-recentf)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                          'helm-calcul-expression)
(global-set-key (kbd "C-h d")                        'helm-info-at-point)
(global-set-key (kbd "C-h i")                        'helm-info)
(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
(global-set-key (kbd "<f1>")                         'helm-resume)
(global-set-key (kbd "C-h C-f")                      'helm-apropos)
(global-set-key (kbd "C-h a")                        'helm-apropos)
(global-set-key (kbd "C-h C-d")                      'helm-debug-open-last-log)
(global-set-key (kbd "<f5> s")                       'helm-find)
(global-set-key (kbd "S-<f2>")                       'helm-execute-kmacro)
(global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
;(global-set-key (kbd "<f11> o")                      'helm-org-agenda-files-headings)
(global-set-key (kbd "C-s")                          'helm-occur)
;; (progn (define-key prog-mode-map (kbd "C-s") 'helm-occur)
;;        (define-key text-mode-map (kbd "C-s") 'helm-occur))

(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-mini)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)
(define-key global-map (kbd "M-g a")                 'helm-do-grep-ag)
(define-key global-map (kbd "M-g g")                 'helm-grep-do-git-grep)
(define-key global-map (kbd "M-g r")                 'helm-rg)
(define-key global-map (kbd "M-g i")                 'helm-gid)
(define-key global-map (kbd "C-x p h")               'helm-projects-history)
;(define-key global-map (kbd "C-x r c")               'helm-addressbook-bookmarks)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocompletion
;;
;; Package `company' provides an in-buffer autocompletion framework.
;; It allows for packages to define backends that supply completion
;; candidates, as well as optional documentation and source code. Then
;; Company allows for multiple frontends to display the candidates,
;; such as a tooltip menu. Company stands for "Complete Anything".
(use-package company
  :ensure t
  :defer 3
  :init

  (defvar radian--company-backends-global
    '(company-capf
      company-files
      (company-dabbrev-code company-keywords)
      company-dabbrev)
    "Values for `company-backends' used everywhere.
If `company-backends' is overridden by Radian, then these
backends will still be included.")

  :bind (;; Remap the standard Emacs keybindings for invoking
         ;; completion to instead use Company. You might think this
         ;; could be put in the `:bind*' declaration below, but it
         ;; seems that `bind-key*' does not work with remappings.
         ([remap completion-at-point] . company-manual-begin)
         ([remap complete-symbol] . company-manual-begin)

         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.

         :map company-active-map

         ;; Make TAB always complete the current selection, instead of
         ;; only completing a common prefix.
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)

         ;; The following are keybindings that only take effect if the
         ;; user has explicitly interacted with Company. Note that
         ;; `:map' from above is "sticky", and applies also below: see
         ;; https://github.com/jwiegley/use-package/issues/334#issuecomment-349473819.

         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company, instead of always
         ;; doing so.
         ("<return>" . company-complete-selection)
         ("RET" . company-complete-selection)

         ;; We then make <up> and <down> abort the completions menu
         ;; unless the user has interacted explicitly. Note that we
         ;; use `company-select-previous' instead of
         ;; `company-select-previous-or-abort'. I think the former
         ;; makes more sense since the general idea of this `company'
         ;; configuration is to decide whether or not to steal
         ;; keypresses based on whether the user has explicitly
         ;; interacted with `company', not based on the number of
         ;; candidates.
         ;;
         ;; Note that M-p and M-n work regardless of whether explicit
         ;; interaction has happened yet, and note also that M-TAB
         ;; when the completions menu is open counts as an
         ;; interaction.
         ("<up>" . company-select-previous)
         ("<down>" . company-select-next))

  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. We
          ;; already remapped those bindings to `company-manual-begin'
          ;; above. Here we make sure that they definitely invoke
          ;; `company-manual-begin' even if a minor mode binds M-TAB
          ;; directly.
          ("M-TAB" . company-manual-begin))

  :config

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Company appears to override our settings in `company-active-map'
  ;; based on `company-auto-complete-chars'. Turning it off ensures we
  ;; have full control.
  (setq company-auto-complete-chars nil)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make the `company-dabbrev' backend fully case-sensitive, to
  ;; improve the UX when working with domain-specific words that have
  ;; particular casing.
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  ;; When candidates in the autocompletion tooltip have additional
  ;; metadata, like a type signature, align that information to the
  ;; right-hand side. This usually makes it look neater.
  (setq company-tooltip-align-annotations t)

  (global-company-mode +1)

  :blackout t)

;; Package `company-prescient' provides intelligent sorting and
;; filtering for candidates in Company completions.
(use-package company-prescient
  :demand t
  :after company
  :config

  ;; Use `prescient' for Company menus.
  (company-prescient-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; version control configuration
;; Feature `vc-hooks' provides hooks for the Emacs VC package. We
;; don't use VC, because Magit is superior in pretty much every way.
(use-package vc-hooks
  :config

  ;; Disable VC. This improves performance and disables some annoying
  ;; warning messages and prompts, especially regarding symlinks. See
  ;; https://stackoverflow.com/a/6190338/3538165.
  (setq vc-handled-backends nil))

;; Package `magit' provides a full graphical interface for Git within
;; Emacs.
(use-package magit
  :ensure t
  :bind (;; This is the primary entry point for Magit. Binding to C-x
         ;; g is recommended in the manual:
         ;; https://magit.vc/manual/magit.html#Getting-Started
         ("C-x g" . magit-status))

  :init

  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))

  :config

  ;; Enable C-c M-g as a shortcut to go to a popup of Magit commands
  ;; relevant to the current file.
  (global-magit-file-mode +1)

  ;; The default location for git-credential-cache is in
  ;; ~/.config/git/credential. However, if ~/.git-credential-cache/
  ;; exists, then it is used instead. Magit seems to be hardcoded to
  ;; use the latter, so here we override it to have more correct
  ;; behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (let* ((xdg-config-home (or (getenv "XDG_CONFIG_HOME")
                                (expand-file-name "~/.config/")))
           (socket (expand-file-name "git/credential/socket" xdg-config-home)))
      (setq magit-credential-cache-daemon-socket socket)))

  ;; Allow pulling with --rebase just once, without needing to
  ;; configure pull.rebase permanently. See
  ;; https://github.com/magit/magit/issues/2597#issuecomment-201392835.
  (magit-define-popup-switch 'magit-pull-popup ?r "Rebase" "--rebase"))

;; Package `gh' provides an Elisp interface to the GitHub API.
(use-package gh
  :ensure t
  ;; Disable autoloads because this package autoloads *way* too much
  ;; code. See https://github.com/sigma/gh.el/issues/95.
  :defer t)

;; Package `magit-gh-pulls' adds a section to Magit which displays
;; open pull requests on a corresponding GitHub repository, if any,
;; and allows you to check them out locally.
(use-package magit-gh-pulls
  :ensure t
  :demand t
  :after magit
  :config (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

  :blackout t)

;; Package `git-commit' allows you to use Emacsclient as a Git commit
;; message editor, providing syntax highlighting and using
;; `with-editor' to allow you to conveniently accept or abort the
;; commit.
(use-package git-commit
  :ensure t
  :init

  (defun git-commit-setup-check-buffer ()
    (and buffer-file-name
         (string-match-p git-commit-filename-regexp buffer-file-name)
         (git-commit-setup)))

  (define-minor-mode global-git-commit-mode
    "Edit Git commit messages.
This global mode arranges for `git-commit-setup' to be called
when a Git commit message file is opened.  That usually happens
when Git uses the Emacsclient as $GIT_EDITOR to have the user
provide such a commit message."
    :group 'git-commit
    :type 'boolean
    :global t
    :init-value t
    :initialize (lambda (symbol exp)
                  (custom-initialize-default symbol exp)
                  (when global-git-commit-mode
                    (add-hook 'find-file-hook 'git-commit-setup-check-buffer)))
    (if global-git-commit-mode
        (add-hook  'find-file-hook 'git-commit-setup-check-buffer)
      (remove-hook 'find-file-hook 'git-commit-setup-check-buffer)))

  :init

  (global-git-commit-mode +1)

  :config

  ;; Max length for commit message summary is 50 characters as per
  ;; https://chris.beams.io/posts/git-commit/.
  (setq git-commit-summary-max-length 50))


(exec-path-from-shell-initialize)

(setq pdf-view-use-unicode-ligther nil)

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (bind-keys :map pdf-view-mode-map
             ("\\" . hydra-pdftools/body)
             ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
             ("g"  . pdf-view-first-page)
             ("G"  . pdf-view-last-page)
             ("l"  . image-forward-hscroll)
             ("h"  . image-backward-hscroll)
             ("j"  . pdf-view-next-page)
             ("k"  . pdf-view-previous-page)
             ("e"  . pdf-view-goto-page)
             ("u"  . pdf-view-revert-buffer)
             ("al" . pdf-annot-list-annotations)
             ("ad" . pdf-annot-delete)
             ("aa" . pdf-annot-attachment-dired)
             ("am" . pdf-annot-add-markup-annotation)
             ("at" . pdf-annot-add-text-annotation)
             ("y"  . pdf-view-kill-ring-save)
             ("i"  . pdf-misc-display-metadata)
             ("s"  . pdf-occur)
             ("b"  . pdf-view-set-slice-from-bounding-box)
             ("r"  . pdf-view-reset-slice))
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(latex-preview-pane-enable)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell
(use-package haskell-mode
  :config

  ;; Disable in-buffer underlining of errors and warnings, since we
  ;; already have them from Flycheck.
  (setq haskell-process-show-overlays nil)

  ;; Enable REPL integration.
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)

  ;; Work around upstream bug, see
  ;; https://github.com/haskell/haskell-mode/issues/1553.

  (setq haskell-process-args-ghci
        '("-ferror-spans" "-fshow-loaded-modules"))

  (setq haskell-process-args-cabal-repl
        '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

  (setq haskell-process-args-stack-ghci
        '("--ghci-options=-ferror-spans -fshow-loaded-modules"
          "--no-build" "--no-load"))

  (setq haskell-process-args-cabal-new-repl
        '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

  ;; Allow `haskell-mode' to use Stack with the global project instead
  ;; of trying to invoke GHC directly, if not inside any sort of
  ;; project.
  (setq haskell-completion-backend 'ghci)
  (setq haskell-process-type 'stack-ghci))

(add-to-list 'load-path "~/src/lsp-haskell")
(add-to-list 'load-path "~/src/lsp-mode")
(add-to-list 'load-path "~/src/lsp-ui")

(require 'lsp-ui)
(require 'lsp-haskell)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'haskell-mode-hook #'lsp-haskell-enable)
(add-hook 'haskell-mode-hook 'flycheck-mode)
(setq lsp-haskell-process-path-hie "hie-wrapper")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dash at point
;;
;; Uses dash to provide detailed documentation look up based on current mode
(use-package dash-at-point)
(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-window
(use-package ace-window
  :config

  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
        aw-dispatch-always t
        aw-dispatch-alist
        '((?x aw-delete-window "Ace - Delete Window")
          (?c aw-swap-window "Ace - Swap Window")
          (?n aw-flip-window)
          (?v aw-split-window-vert "Ace - Split Vert Window")
          (?h aw-split-window-horz "Ace - Split Horz Window")
          (?m delete-other-windows "Ace - Maximize Window")
          (?g delete-other-windows)
          (?b balance-windows)
          (?u (lambda ()
                (progn
                  (winner-undo)
                  (setq this-command 'winner-undo))))
          (?r winner-redo))))

;; Ace-window hydra controls - for the lazy
;; Trigger with 'M-o w'
(defhydra hydra-window ()
      "
Movement^^	^Split^		^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←		_v_ertical	_b_uffer		_q_ X←
_j_ ↓		_x_ horizontal	_f_ind files	_w_ X↓
_k_ ↑		_z_ undo		_a_ce 1		_e_ X↑
_l_ →		_Z_ reset		_s_wap		_r_ X→
_F_ollow		_D_lt Other	_S_ave		max_i_mize
_SPC_ cancel	_o_nly this	_d_elete
"
  ("h" windmove-left )
  ("j" windmove-down )
  ("k" windmove-up )
  ("l" windmove-right )
  ("q" hydra-move-splitter-left)
  ("w" hydra-move-splitter-down)
  ("e" hydra-move-splitter-up)
  ("r" hydra-move-splitter-right)
  ("b" helm-mini)
  ("f" helm-find-files)
  ("F" follow-mode)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body))
   )
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))
   )
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down))
   )
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("S" save-buffer)
  ("d" delete-window)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body))
   )
  ("o" delete-other-windows)
  ("i" ace-maximize-window)
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo))
   )
  ("Z" winner-redo)
  ("SPC" nil))
(add-to-list 'aw-dispatch-alist '(?w hydra-window/body) t)
(global-set-key (kbd "M-o") 'ace-window)

;; Hydra for buffer o' buffers
;; Just press '.' to bring it up
(defhydra hydra-ibuffer-main (:color pink :hint nil)
    "
^Mark^         ^Actions^         ^View^          ^Select^              ^Navigation^
_m_: mark      _D_: delete       _g_: refresh    _q_: quit             _k_:   ↑    _h_
_u_: unmark    _s_: save marked  _S_: sort       _TAB_: toggle         _RET_: visit
_*_: specific  _a_: all actions  _/_: filter     _o_: other window     _j_:   ↓    _l_
_t_: toggle    _._: toggle hydra _H_: help       C-o other win no-select
"
  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)
  ("t" ibuffer-toggle-marks)

  ("D" ibuffer-do-delete)
  ("s" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("S" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)
  ("H" describe-mode :color blue)

  ("h" ibuffer-backward-filter-group)
  ("k" ibuffer-backward-line)
  ("l" ibuffer-forward-filter-group)
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)

  ("TAB" ibuffer-toggle-filter-group)

  ("o" ibuffer-visit-buffer-other-window :color blue)
  ("q" quit-window :color blue)
  ("." nil :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                                     :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                       :after-exit
                                       (if (eq major-mode 'ibuffer-mode)
                                           (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

;; toggle the hydra with '.'
(define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)
;; Automatically open the hydra when ibuffer opens
(add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sane defaults, please
(setq inhibit-startup-screen t                ;; the welcome screen is for guests only, I'm at home now!
      initial-scratch-message nil             ;; remove the message in the scratch buffer
      visible-bell t                          ;; remove the annoying beep
      apropos-do-all t                        ;; apropos commands perform more extensive searches than default
      large-file-warning-threshold 100000000) ;; warn only when opening files bigger than 100MB

;; no bars, no gui menus
(tool-bar-mode   -1)
(scroll-bar-mode -1)

;; replace yes/no questions with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; show the empty lines at the end (bottom) of the buffer
(toggle-indicate-empty-lines)

;; delete the previous selection when overrides it with a new insertion.
(delete-selection-mode)

;; the blinking cursor is pretty annoying, so disable it.
(blink-cursor-mode -1)

;; more thinner window divisions
(fringe-mode '(1 . 1))

;; use ibuffer by default
(defalias 'list-buffers 'ibuffer)

;; make sure that UTF-8 is used everywhere.
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

 ;; always indent with spaces
(setq-default indent-tabs-mode  nil
              default-tab-width 4
              c-basic-offset    2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up some temporary folders
(defvar jp/emacs-temp-directory (concat user-emacs-directory "tmp/"))
(unless (file-exists-p jp/emacs-temp-directory)
  (make-directory jp/emacs-temp-directory))

;; Change the autosave location
(defvar jp/emacs-autosave-directory (concat user-emacs-directory "auto-save/"))
(unless (file-exists-p jp/emacs-autosave-directory)
  (make-directory jp/emacs-autosave-directory))
(setq backup-directory-alist `(("." . ,jp/emacs-autosave-directory)))
(setq auto-save-list-file-prefix jp/emacs-autosave-directory)
(setq auto-save-file-name-transforms `((".*" ,jp/emacs-autosave-directory t)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )




;; Let's keep some command history
(setq-default history-length 1000)
(setq savehist-file (concat jp/emacs-temp-directory "history")
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
(savehist-mode t)

;; Keep a history of recently accessed files
(use-package recentf
  :config
  (progn
    (setq recentf-save-file (concat jp/emacs-temp-directory "recentf")
          recentf-max-saved-items 100
          recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG"))
    (recentf-mode t)))
(add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))

(setq-default tramp-persistency-file-name (concat jp/emacs-temp-directory "tramp")             ;; Tramp history
              bookmark-default-file (concat jp/emacs-temp-directory "bookmarks")               ;; Bookmarks file
              semanticdb-default-save-directory (concat jp/emacs-temp-directory "semanticdb")  ;; SemanticDB files
              url-configuration-directory (concat jp/emacs-temp-directory "url")               ;; url files
              eshell-directory-name (concat jp/emacs-temp-directory "eshell" )                 ;; eshell files
              )

;; history of recent actions
(setq-default history-length 1000)
(setq savehist-file (concat jp/emacs-temp-directory "history")
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
(savehist-mode t)

;; History of recent files
(use-package recentf
  :config
  (progn
    (setq recentf-save-file (concat jp/emacs-temp-directory "recentf")
          recentf-max-saved-items 100
          recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG"))
    (recentf-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp goodness
(defun close-all-parentheses ()
  (interactive "*")
  (let ((closing nil))
    (save-excursion
      (while (condition-case nil
                 (progn
                   (backward-up-list)
                   (let ((syntax (syntax-after (point))))
                     (case (car syntax)
                           ((4) (setq closing (cons (cdr syntax) closing)))
                           ((7 8) (setq closing (cons (char-after (point)) closing)))))
                   t)
               ((scan-error) nil))))
    (apply #'insert (nreverse closing))))

(define-key global-map (kbd "C-c [") 'close-all-parentheses)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up base folders for documents and PDFs and such...
(defvar jp/docs (expand-file-name "~/Documents/") "Documents folder")
(defvar jp/papers-base (concat jp/docs "reading/") "Location for reading library including PDFs, bibliography, and notes")
(defvar jp/papers-pdfs (concat jp/papers-base "lib/") "PDF folder")
(defvar jp/papers-notes (concat jp/papers-base "index.org") "Default location for notes about papers")
(defvar jp/papers-refs  (concat jp/papers-base "index.bib") "Bibliography")
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
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
(use-package org
  ;:defer 1
  :ensure org-plus-contrib
  :config
  (progn
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
    (setq org-directory (concat jp/docs "org/"))
    (setq jp/org-inbox-file (concat org-directory "inbox.org"))
    (setq org-default-notes-file jp/org-inbox-file)

    ;; set the archive
    (setq org-agenda-custom-commands
          '(("Q" . "Custom queries") ;; gives label to "Q"
            ("Qa" "Archive search" search ""
             ((org-agenda-files (file-expand-wildcards (concat org-directory "*.org_archive")))))
            ;; ...other commands here
            ))

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
    ;; Add minted to the defaults packages to include when exporting.
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (add-to-list 'org-latex-packages-alist '("" "xunicode"))
    ;; Tell the latex export to use the minted package for source
    ;; code coloration.
    (setq org-latex-listings 'minted)
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
          org-agenda-files (list
                             (concat org-directory "agenda.org")                          ;; Some kind of actual agenda
                             (concat org-directory "inbox.org")                           ;; A dumping ground
                             (concat org-directory "index.org")                           ;; The larger org-mode project list and general purpose index
                             (concat org-directory "geu.org")                             ;; agitating for labor
                             (concat org-directory "333.org")                             ;; CS333 - fun for you, fun for me
                             (concat org-directory "calsync/jeremiahpeschka-cal.org")
                             (concat org-directory "calsync/legitbiz-cal.org")
                             (concat org-directory "calsync/jpeschka-cal.org"))
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
    ;; Custom agenda views
    (setq org-agenda-custom-commands
          `(("u" "Unscheduled TODO items"
             (lambda (org-match)
               (find-file org-todo-file)
               (org-match-sparse-tree 'todo-only "-DEADLINE={.}/!")))
            ("t" "All TODO items"
             (lambda (org-match)
               (find-file org-todo-file)
               (org-match-sparse-tree 'todo-only "/!")))
            ("a" "Agenda" agenda)))

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
    (display-time)                        ;; activate time display

    (org-agenda-to-appt)                  ;; generate the appt list from org agenda files on emacs launch
    (run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
    (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

    ;; set up the call to terminal-notifier
    (defvar my-notifier-path
      "/usr/local/bin/terminal-notifier")
    (defun my-appt-send-notification (title msg)
      (shell-command (concat my-notifier-path " -message " msg " -title " title " -sender org.gnu.Emacs ")))

    ;; designate the window function for my-appt-send-notification
    (defun my-appt-display (min-to-app new-time msg)
      (my-appt-send-notification
       (format "'Appointment in %s minutes'" min-to-app)    ;; passed to -title in terminal-notifier call
       (format "'%s'" msg)))                                ;; passed to -message in terminal-notifier call
    (setq appt-disp-window-function (function my-appt-display))




    ;; t - Prompt for a title and then add to notes.org unless you refile it
    (setq org-capture-templates
          '(("t" "todo" entry (file jp/org-inbox-file)
             "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
            ("m" "Meeting" entry (file org-default-notes-file)
             "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
            ("i" "Idea" entry (file jp/org-inbox-file)
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

    (defun jp/org-template ()
      (insert "#+AUTHOR: Jeremiah Peschka
#+EMAIL: jeremiah.peschka@gmail.com
#+STARTUP: indent showall
#+OPTIONS: tags:nil")
      (org-mode-restart))

    (define-auto-insert "\\.org$" #'jp/org-template)


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
  :after org)
(use-package doi-utils
  :after org-ref)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOAR ORG
;;
;; enable auto-fill-mode for org
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;; org-journal gives C-c C-j to create a new journal entry
(use-package org-journal)
;; pretty bullets
(use-package org-bullets)
;; always pretty bullets
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keyboard config
;; zoooooooooom
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
;; Move the entire buffer up or down one line at a time.
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
;; Disables suspend keys so we aren't locking up emacs in a GUI
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
;; No accidental minimize or quit on macOS
(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "s-q"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp settings
;;
;; ZSH on the PDX servers caused problems with tramp hanging.
;; Force to bash instead to make life simple.
;;
;; Mon Jun 19 17:43:56 PDT 2017
;; I'm not sure but this _might_ be causing problems for my local TRAMP mode
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font ligatures
;;
;; TODO Replace this with a check that either loads mac-auto-operator-composition-mode
;;      -OR- uses prettify symbols
(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

(set-default-font "PragmataPro Liga 16" t t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macOS: Raise emacs on activation
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
  (mapc (lambda (pair) (push pair prettify-symbols-alist))
        ;; Need a way to make this work with comments as well.
        '(
          ("\\" . ?λ)
          ("*" . ?⋅)
          ("forall" . ?∀)
          ("forAll" . ?∀)
          ("all"    . ?∀)
          ("exists" . ?∃)
          ("undefined" . ?⊥)
          ("elem" . ?∈)
          ("flip elem" . ?∋)
          ("notElem" . ?∉)
          ("flip notElem" . ?∌)
          ("member" . ?∈)
          ("notMember" . ?∉)
          ("union" . ?⋃)
          ("intersection" . ?⋂)
          ("isSubsetOf" . ?⊆)
          ("isProperSubsetOf" . ?⊂)
          (" . " . (? (Br . Bl) ?◦ (Br . Bl) ? ))
          ("/" . ?÷)
          ("div" . ?÷)
          ("quot" . ?÷))))

(add-hook 'haskell-mode-hook #'fp-prettify-symbols)

(defun add-pragmatapro-prettify-symbols-alist ()
  (dolist (alias pragmatapro-prettify-symbols-alist)
    (push alias prettify-symbols-alist)))

(add-hook 'prog-mode-hook
          #'add-pragmatapro-prettify-symbols-alist)

(global-prettify-symbols-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable smooth scrolling
(if mac-mouse-wheel-smooth-scroll
    (setq mac-mouse-wheel-smooth-scroll nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load theme as the very last activity
;; (load-theme 'challenger-deep t)
(load-theme 'tangotango t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("713f898dd8c881c139b62cf05b7ac476d05735825d49006255c0a31f9a4f46ab" "f71859eae71f7f795e734e6e7d178728525008a28c325913f564a42f74042c31" default)))
 '(package-selected-packages
   (quote
    (benchmark-init org-ref interleave latex-preview-pane smartparens markdown-mode dash-functional dash-at-point org-super-agenda exec-path-from-shell flycheck flycheck-haskell haskell-mode gh magit magit-gh-pulls company company-cabal company-lsp company-prescient restart-emacs helm-rg ace-window helm helm-bibtex org org-bullets org-journal org-plus-contrib pdf-tools which-key use-package challenger-deep-theme rainbow-delimiters hydra))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
