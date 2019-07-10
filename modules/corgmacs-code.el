;;; -*- lexical-binding: t -*-
;;; corgmacs-code.el

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
;;; General purpose code stuff that doesn't get its own file

;;; Code:
;;; Code goes here, moron.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
(use-package flycheck
  :ensure t)

(add-hook 'prog-mode-hook #'flycheck-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake
;; Don't put flymake copies of files in the working folder.
(setq flymake-run-in-place nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile
(use-package projectile
  :ensure t
  :defer 3
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-search-path '("~/src/peschkaj/"
                                         "~/src/"))
  (projectile-mode))


(defun jp-eval-after-load-grep ()
  "Get grep to ignore tags, build files, and the seL4 build directory."
  (add-to-list 'grep-find-ignored-directories "build-ia32")
  (add-to-list 'grep-find-ignored-files "tags")
  (add-to-list 'grep-find-ignored-files "GTAGS")
  (add-to-list 'grep-find-ignored-files "GRTAGS")
  (add-to-list 'grep-find-ignored-files "*.ninja"))

(use-package helm-projectile
  :ensure t
  :after helm
  :config
  (helm-projectile-on)
  (eval-after-load "grep"
    '(jp-eval-after-load-grep))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp goodness
(defun close-all-parentheses ()
  "Closes all parenthesis."
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
;; Dash at point
;;
;; Uses dash to provide detailed documentation look up based on current mode
(when (memq window-system '(mac ns))
  (use-package dash-at-point)
  (global-set-key "\C-cd" 'dash-at-point)
  (global-set-key "\C-ce" 'dash-at-point-with-docset))



(provide 'corgmacs-code)
;;; corgmacs-code.el ends here
