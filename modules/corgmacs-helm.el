;;; -*- lexical-binding: t -*-
;;; corgmacs-helm.el

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
;; helm
(use-package helm
  :bind (:map helm-map
              ("<tab>" . helm-execute-persistent-action))
  :config
  (require 'helm-config)
  (setq helm-mode-fuzzy-match                 t ;; enable fuzzy match
        helm-M-x-fuzzy-match                  t
        helm-recentf-fuzzy-match              t
        helm-buffers-fuzzy-matching           t
        helm-ff-fuzzy-matching                t
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
  (global-unset-key (kbd "C-s"))                       ; unset C-s for isearch, rebind to C-s s
  (global-set-key (kbd "C-s s")                        'isearch-forward)
  (global-set-key (kbd "C-s C-s")                      'helm-occur)
  (global-set-key (kbd "C-s o")                        'helm-occur-from-isearch)
  ;; (global-set-key (kbd "<f11> o")                      'helm-org-agenda-files-headings)
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
  (helm-mode 1)
  :blackout t)
(use-package helm-bibtex
  :ensure t)



(provide 'corgmacs-helm)
;;;
