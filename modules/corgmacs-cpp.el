;;; -*- lexical-binding: t -*-
;;; corgmacs-cpp.el

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
;;; corgmacs-cpp for all your undefined behavior needs

;;; Code:
;;; Code goes here, moron.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ configuration
(use-package google-c-style
  :ensure t
  :config
  (add-hook 'c-common-hook 'google-set-c-style))

;; Set up clang-format
(use-package clang-format
  :ensure t
  :config
  (global-set-key (kbd "C-c i") 'clang-format-region)
  (global-set-key (kbd "C-c u") 'clang-format-buffer)
  (setq clang-format-style-option "file"
        c-c++-enable-clang-support t))

;; uses Allman style by default
(setq c-default-style "bsd"
      c-basic-offset 2)

;; nobody likes tabs
(setq-default indent-tabs-mode nil)
;; and tabs should only be 2 characters
(setq-default tab-width 2)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; Some better smartparens behavior:
;; when you press RET, the curly braces automatically
;; add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))

;; stop smartparens from getting drunk and always escaping single quotes
(setq sp-escape-quotes-after-insert nil)

;; By default, Emacs won't indent when press RET because the command bound to
;; RET is newline. You can enable automatic indentation by binding RET to
;; newline-and-indent.
(global-set-key (kbd "RET") 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp-mode for C/C++
(use-package lsp-mode
  :ensure t
  :hook (c-mode . lsp)
  :commands (lsp lsp-deferred))

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package company-lsp
  :commands company-lsp)
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)
;; We don't currently use treemacs
;; (use-package lsp-treemacs
;;   :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
;;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package cc-mode)





(provide 'corgmacs-cpp)
;;; corgmacs-cpp.el ends here
