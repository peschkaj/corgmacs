;;; -*- lexical-binding: t -*-
;;; corgmacs-vc.el

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
;;; Please don't provide any, this is garbage.

;;; Code:
;;; Code goes here, moron.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; version control configuration
;; Feature `vc-hooks' provides hooks for the Emacs VC package. We
;; don't use VC, because Magit is superior in pretty much every way.
(use-package vc-hooks
  :config

  ;; Disable VC. This improves performance and disables some annoying
  ;; warning messages and prompts, especially regarding symlinks. See
  ;; https://stackoverflow.com/a/6190338/3538165.
  (remove-hook 'find-file-hook 'vc-find-file-hook))

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
  ;;(magit-define-popup-switch 'magit-pull-popup ?r "Rebase" "--rebase")
  )

;; Package `gh' provides an Elisp interface to the GitHub API.
(use-package gh
  :ensure t
  ;; Disable autoloads because this package autoloads *way* too much
  ;; code. See https://github.com/sigma/gh.el/issues/95.
  :defer t)

;; Package `magit-gh-pulls' adds a section to Magit which displays
;; open pull requests on a corresponding GitHub repository, if any,
;; and allows you to check them out locally.
;; (use-package magit-gh-pulls
;;   :ensure t
;;   :demand t
;;   :after magit
;;   :config (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;;   :blackout t)

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

(use-package exec-path-from-shell
    :ensure t
    :config(exec-path-from-shell-initialize))

;; (when (memq window-system '(mac ns))
;;   (use-package exec-path-from-shell
;;     :ensure t
;;     :config(exec-path-from-shell-initialize)))

(when (window-system)
  ;; Disables pdf-tools using unicode symbols on the mode line
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
    (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)))


(provide 'corgmacs-vc)
;;; corgmacs-vc
