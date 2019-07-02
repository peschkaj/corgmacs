;;; -*- lexical-binding: t -*-
;;; corgmacs-fstar.el

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
;;; keeping fstar happy

;;; Code:
;;; Code goes here, moron.


(use-package fstar-mode
  :ensure t
  :config
  (setq-default fstar-executable "/home/jeremiah/src/fstarlang/FStar//bin/fstar.exe"
                fstar-smt-executable "/usr/bin/z3")
  )

(provide 'corgmacs-fstar)
;;; corgmacs-fstar.el ends here
