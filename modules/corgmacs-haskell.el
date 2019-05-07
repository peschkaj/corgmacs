;;; -*- lexical-binding: t -*-
;;; corgmacs-haskell.el

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

(use-package intero
  :ensure t
  :defer t
  :commands intero-mode)

(require 'brittany)

(use-package haskell-mode
  :ensure t
  :defer t
  :commands haskell-mode
  :config (progn
            (setq haskell-stylish-on-save t
                  haskell-mode-stylish-haskell-path "brittany"
                  haskell-tags-on-save t
                  ;; Allow `haskell-mode' to use Stack with the global project instead
                  ;; of trying to invoke GHC directly, if not inside any sort of
                  ;; project.
                  haskell-process-type 'stack-ghci
                  ;; Work around upstream bug, see
                  ;; https://github.com/haskell/haskell-mode/issues/1553.
                  haskell-process-args-ghci '("-ferror-spans" "-fshow-loaded-modules")
                  haskell-process-args-cabal-repl '("--ghc-options=-ferror-spans -fshow-loaded-modules")
                  haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans -fshow-loaded-modules" "--no-build" "--no-load")
                  haskell-process-args-cabal-new-repl '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
            ;; Sets up haskell-mode to use intero for completion
            (add-hook 'haskell-mode-hook 'intero-mode)
            (add-hook 'haskell-mode-hook 'brittany-mode)
            (add-hook 'haskell-mode-hook #'smartparens-mode)
            ;; Enable REPL integration
            (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
            ;; enable flycheck
            (add-hook 'haskell-mode-hook 'flycheck-mode)
            ;; Set up keybinds
            (define-key haskell-mode-map (kbd "C-c h i g") 'haskell-navigate-imports)
            (define-key haskell-mode-map (kbd "C-c h i f") 'haskell-mode-format-imports)
            (define-key haskell-mode-map (kbd "C-c h i s") 'haskell-sort-imports)
            (define-key haskell-mode-map (kbd "C-c h i a") 'haskell-align-imports)
            (define-key haskell-mode-map (kbd "C-c h s b") 'haskell-mode-stylish-buffer))
  :bind (("<Tab>" . brittany-reformat-region)))

(use-package company-ghci
  :ensure t
  :defer t)

(use-package helm-hoogle
  :ensure t
  :defer t)

(use-package hindent
  :ensure t
  :defer t
  :init (add-hook 'haskell-mode-hook #'hindent-mode)
  :bind (("C-c h ")))

(use-package hlint-refactor
  :ensure t
  :defer t
  :bind (("C-c h r b" . hlint-refactor-refactor-buffer)
         ("C-c h r r" . hlint-refactor-refactor-at-point)))

(provide 'corgmacs-haskell)
;;; haskell.el ends here
