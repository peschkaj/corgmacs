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
;; better beginning of line
;;
;; From https://github.com/howardabrams/dot-files/blob/master/emacs-fixes.org
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
(global-set-key [remap org-beginning-of-line]  'smarter-move-beginning-of-line)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace cleanup on save
(add-hook 'before-save-hook 'whitespace-cleanup)



(provide 'corgmacs-editing)
;;; end of editing
