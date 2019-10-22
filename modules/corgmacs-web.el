;;; -*- lexical-binding: t -*-
;;; corgmacs-web.el

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
;;; Web development tools

;;; Code:
;;; Code goes here, moron.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS
(use-package rainbow-mode
  :ensure t)

(use-package css-eldoc
  :ensure t
  :defer t
  :config
  (add-hook 'css-mode-hook 'turn-on-css-eldoc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript
(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js2-mode))
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

(use-package js2-refactor
  :ensure t
  :after js2-mode
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (setq-default js-indent-level 2))

(use-package eslint-fix
  :ensure t
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))))

(use-package json-mode
  :ensure t)

(use-package prettier-js
  :ensure t)

(use-package simple-httpd
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live preview of web pages
;;
;; See https://github.com/skeeto/skewer-mode for keybinds
(use-package skewer-mode
  :ensure t)

(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browse current HTML file
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(provide 'corgmacs-web)
;;; corgmacs-web.el ends here
