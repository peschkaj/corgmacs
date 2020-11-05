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

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Set up list of package repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; avoid problems with files newer than their byte-compiled
;; counterparts it's better to have a slower startup than load an
;; outdated and maybe bugged package
(setq load-prefer-newer t)
;; initialize the packages and create the packages list if not exists
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create a new custom-file
;;
;; Having our own custom file means emacs doesn't poop all over this file and
;; constantly generate false changes for version control.
(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; benchmark-init
;;
;; Uncomment this if emacs is being slow on startup.
;; View important timing stats with M-x benchmark-init/show-durations-tabluation
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

(eval-when-compile
  (require 'use-package))

(use-package ibuffer
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up loading from modules directory
;;
;; This code is borrowed from Bodil Stokke's excell ohai-emacs
;; see https://github.com/bodil/ohai-emacs for the original.
;;
;; Figure out the path to our .emacs.d by getting the path part of the
;; current file (`init.el`).
(defvar corgmacs/dotfiles-dir (file-name-directory
                               (or (buffer-file-name) (file-chase-links load-file-name))))

;; Individual modules are stored in a `modules` subdirectory of .emacs.d; it's
;; necessary to add these modules explicitly to the load-path.
(add-to-list 'load-path (concat corgmacs/dotfiles-dir "modules"))

;; TODO add check that this file exists
(let ((corgmacs/local-customizations
      (expand-file-name "corgmacs-custom.el" corgmacs/dotfiles-dir)))
  (if (file-readable-p corgmacs/local-customizations)
      (load-file corgmacs/local-customizations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp settings
;;
;; If you experience tramp hangs, they can fixed by adding the following AT THE
;; VERY END of .zshrc (or .zshrc.local):
;; [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
(use-package tramp
  :init (setq tramp-ssh-controlmaster-options nil))

(defvar corgmacs/notifier-path
  "/usr/bin/notify-send"
  "Notifier program. Can be overriden via ~/.emacs-custom.el")

(let ((corgmacs/os-customizations
       (cond ((or (eq system-type 'windows-nt)
                  (eq system-type 'w32))
              (expand-file-name "cormacs-windows.el" (concat corgmacs/dotfiles-dir "modules")))
             ((eq system-type 'darwin)
              (expand-file-name "corgmacs-macos.el" (concat corgmacs/dotfiles-dir "modules")))
             (t (expand-file-name "corgmacs-linux.el" (concat corgmacs/dotfiles-dir "modules")))
             )))
  (if (file-readable-p corgmacs/os-customizations)
      (load-file corgmacs/os-customizations)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings
;;
;; Includes:
;;  - Sane defaults for warnings
;;  - Time formatted to a 24 hour clock
;;  - Removal of tool, menu, and scroll bars
;;  - Setting up a temporary and autosave location
;;  - Setting up sane defaults around autosaving files
(require 'corgmacs-general)
(require 'corgmacs-editing)
(require 'corgmacs-helm)
(require 'corgmacs-vc)
(require 'corgmacs-code)
(require 'corgmacs-lisp)
(require 'corgmacs-cpp)
(require 'corgmacs-fstar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LLVM tools
(if (or (not (eq system-type 'windows-nt))
        (not (eq system-type 'w32)))
    (setq load-path
          (cons (expand-file-name "/usr/share/emacs/site-lisp/llvm-8")
                (cons (expand-file-name "/usr/share/emacs/site-lisp/emacs-llvm-mode") load-path)))
  (require 'llvm-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell
;; (require 'corgmacs-haskell)


(require 'corgmacs-org)


(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))


(cond ((or (eq system-type 'windows-nt)
           (eq system-type 'w32))
       (require 'cascadia-code-mode))
      (t (require 'corgmacs-linux-iosevka)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown
(use-package markdown-mode
  :ensure t
  :config (autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyfreq
;;
;; tracks how often commands are used without a shortcut
;; view usage with keyfreq-show
(use-package keyfreq
  :ensure t
  :defer 2
  :blackout t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configuration for spaceline + spaceline-all-the-icons
(use-package all-the-icons
  :ensure t
  )

;; (use-package nyan-mode
;;   :ensure t
;;   :config (nyan-mode))

(use-package git-gutter
  :ensure t
  :defer 1
  :config
  (add-hook 'prog-mode-hook #'git-gutter-mode))

;; (use-package fancy-battery
;;   :ensure t
;;   :config (fancy-battery-mode))

(use-package spaceline
  :ensure t
  :after all-the-icons)

(use-package spaceline-config
  :ensure spaceline
  :after spaceline
  :config
  (setq powerline-default-separator 'bar))

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config

  (setq spaceline-all-the-icons-separator-type 'none)
  ;(setq spaceline-all-the-icons-separator-type 'arrow)
  (setq spaceline-all-the-icons-icon-set-modified 'circle)

  (spaceline-toggle-all-the-icons-vc-icon-on)
  (spaceline-toggle-all-the-icons-mode-icon-on)
  (spaceline-toggle-all-the-icons-vc-status-on)
  (spaceline-toggle-all-the-icons-text-scale-on)
  ;; nyan takes care of this
  (spaceline-toggle-all-the-icons-hud-on)
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
(if (not (or (eq system-type 'windows-nt)
             (eq system-type 'w32)))
    (use-package multi-term
      :ensure t
      :defer 3
      :config
      (add-hook 'term-mode-hook (lambda ()
                                  (define-key term-raw-map (kbd "C-y") 'term-paste)))
      (setq multi-term-program "/usr/local/bin/zsh")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing tools
(use-package writegood-mode
  :ensure t
  :defer 2
  :config (add-hook 'text-mode-hook 'writegood-mode))

(use-package langtool
  :ensure t
  :defer 2
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
(load-theme 'zerodark)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global blackouts
(blackout 'auto-revert-mode)



;; Start up a server for great fun
(server-start)

;; Revert to a more sane gc threshold to keep pauses as short as possible.
(setq gc-cons-threshold (* 2 1000 1000))



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
