;;; -*- lexical-binding: t -*-
;;; corgmacs-editing.el

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
;;; General editing modules and stuff

;;; Code:
;;; Code goes here, moron.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pretty parens and curly bois
;;
;; Highlight the matching buddy to the current paren/bracket/curly boi
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; set up rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook
            #'rainbow-delimiters-mode))


;; smartparens cheatsheet can be found at
;; https://gist.github.com/pvik/8eb5755cc34da0226e3fc23a320a3c95
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (add-hook 'tuareg-mode-hook  #'smartparens-mode)
  (add-hook 'c-mode-hook       #'smartparens-mode)
  (add-hook 'latex-mode-hook   #'smartparens-mode)
  (add-hook 'rust-mode-hook    #'smartparens-mode)
  (add-hook 'racket-mode-hook  #'smartparens-mode))


(use-package paredit
  :ensure t
  :blackout
  :config
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo tree
;;
;; Rebind the undo and redo commands to undo tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (global-unset-key (kbd "C-/"))
  (global-set-key (kbd "C-/") 'undo)
  (global-set-key (kbd "C-?") 'undo-tree-redo))


(provide 'corgmacs-editing)
;;; end of editing
