;;; -*- lexical-binding: t -*-
;;; corgmacs-linux.el

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
;;; Custom paths or whatever for Linux

;;; Code:
;;; Code goes here, moron.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font tweaks
(set-default-font "PragmataPro 12" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notifications
;;
;; Can be overridden in ~/.emacs-custom.el
(if (not (boundp 'corgmacs/notifier-path))
    (setq corgmacs/notifier-path "/usr/bin/notify-send"))

(defun corgmacs/appt-send-notification (title msg)
  (shell-command (concat corgmacs/notifier-path
			 " -a emacs "
			 " " title
			 " " msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Changes
;;
;; emacs on linux doesn't like to play nicely with changes to the keyboard
;; layout when performed via `.xinitrc`. In theory, this would let us swap super
;; and alt on linux, but a buttload of stuff is bound to meta via xmonad, so the
;; first step is going to be to change the xmonad config. Once that's changed,
;; then it should be possible to use the following to swap meta and super. Until
;; then, enjoy the RSI...
(setq  x-meta-keysym  'super
       x-super-keysym 'meta)

(provide 'corgmacs-linux)
;;; corgmacs-linux.el ends here
