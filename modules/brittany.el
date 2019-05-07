;;; brittany.el --- Wrapper for brittany

;; Copyright (c) 2018 Tony Day. All rights reserved.

;; Author: Tony Day <tonyday567@gmail.com>
;; URL: https://github.com/tonyday/brittany
;; Package-Requires: ((cl-lib "0.5"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Based on hindent

;; Provides a minor mode and commands for easily using the "brittany"
;; program to reformat Haskell code.

;; Add `brittany-mode' to your `haskell-mode-hook' and use the provided
;; keybindings as needed.

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization properties

(defgroup brittany nil
  "Integration with \"brittany\"."
  :prefix "brittany-"
  :group 'haskell)

(defcustom brittany-process-path
  "brittany"
  "Location where the brittany executable is located."
  :group 'brittany
  :type 'string
  :safe #'stringp)

(defcustom brittany-extra-args nil
  "Extra arguments to give to brittany"
  :group 'brittany
  :type 'sexp
  :safe #'listp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode

(defvar brittany-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap indent-region] #'brittany-reformat-region)
    map)
  "Keymap for `brittany-mode'.")

;;;###autoload
(define-minor-mode brittany-mode
  "Indent code with the brittany program.
Provide the following keybindings:
\\{brittany-mode-map}"
  :init-value nil
  :keymap brittany-mode-map
  :lighter " B"
  :group 'brittany
  :require 'brittany)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

;;;###autoload
(defun brittany-reformat-buffer ()
  "Reformat the whole buffer."
  (interactive)
  (brittany-reformat-region (point-min)
                           (point-max)))

;;;###autoload
(defun brittany-reformat-region (beg end &optional drop-newline)
  "Reformat the region from BEG to END, accounting for indentation.
If DROP-NEWLINE is non-nil, don't require a newline at the end of
the file."
  (interactive "r")
  (let ((inhibit-read-only t))
    (if (= (save-excursion (goto-char beg)
                           (line-beginning-position))
           beg)
        (brittany-reformat-region-as-is beg end drop-newline)
      (let* ((column (- beg (line-beginning-position)))
             (string (buffer-substring-no-properties beg end))
             (new-string (with-temp-buffer
                           (insert (make-string column ? ) string)
                           (brittany-reformat-region-as-is (point-min)
                                                          (point-max)
                                                          drop-newline)
                           (delete-region (point-min) (1+ column))
                           (buffer-substring (point-min)
                                             (point-max)))))
        (save-excursion
          (goto-char beg)
          (delete-region beg end)
          (insert new-string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal library

(defun brittany-reformat-region-as-is (beg end &optional drop-newline)
  "Reformat the given region from BEG to END as-is.
This is the place where brittany is actually called.
If DROP-NEWLINE is non-nil, don't require a newline at the end of
the file."
  (let* ((original (current-buffer))
         (orig-str (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (let ((temp (current-buffer)))
        (with-current-buffer original
          (let ((ret (apply #'call-process-region
                            (append (list beg
                                          end
                                          brittany-process-path
                                          nil ; delete
                                          temp ; output
                                          nil)
                                    (brittany-extra-arguments)))))
            (cond
             ((= ret 1)
              (let ((error-string
                     (with-current-buffer temp
                       (let ((string (progn (goto-char (point-min))
                                            (buffer-substring (line-beginning-position)
                                                              (line-end-position)))))
                         string))))
                (if (string= error-string "brittany: Parse error: EOF")
                    (message "language pragma")
                  (error error-string))))
             ((= ret 0)
              (let* ((last-decl (= end (point-max)))
                     (new-str (with-current-buffer temp
                                (when (and drop-newline (not last-decl))
                                  (goto-char (point-max))
                                  (when (looking-back "\n" (1- (point)))
                                    (delete-char -1)))
                                (buffer-string))))
                (if (not (string= new-str orig-str))
                    (let ((line (line-number-at-pos))
                          (col (current-column)))
                      (delete-region beg
                                     end)
                      (let ((new-start (point)))
                        (insert new-str)
                        (let ((new-end (point)))
                          (goto-char (point-min))
                          (forward-line (1- line))
                          (goto-char (+ (line-beginning-position) col))
                          (when (looking-back "^[ ]+" (line-beginning-position))
                            (back-to-indentation))
                          (delete-trailing-whitespace new-start new-end)))
                      (message "Formatted."))
                  (message "Already formatted.")))))))))))

(defun brittany-decl-points ()
  "Get the start and end position of the current declaration.
This assumes that declarations start at column zero and that the
rest is always indented by one space afterwards, so Template
Haskell uses with it all being at column zero are not expected to
work."
  (cond
   ;; If we're in a block comment spanning multiple lines then let's
   ;; see if it starts at the beginning of the line (or if any comment
   ;; is at the beginning of the line, we don't care to treat it as a
   ;; proper declaration.
   ((and (brittany-in-comment)
         (save-excursion (goto-char (line-beginning-position))
                         (brittany-in-comment)))
    nil)
   ((save-excursion
      (goto-char (line-beginning-position))
      (or (looking-at "^-}$")
          (looking-at "^{-$")))
    nil)
   ;; Otherwise we just do our line-based hack.
   (t
    (save-excursion
      (let ((start
             (or (cl-letf
                     (((symbol-function 'jump)
                       #'(lambda ()
                           (search-backward-regexp "^[^ \n]" nil t 1)
                           (cond
                            ((save-excursion (goto-char (line-beginning-position))
                                             (looking-at "|]"))
                             (jump))
                            (t (unless (or (looking-at "^-}$")
                                           (looking-at "^{-$"))
                                 (point)))))))
                   (goto-char (line-end-position))
                   (jump))
                 0))
            (end
             (progn
               (goto-char (1+ (point)))
               (or (cl-letf
                       (((symbol-function 'jump)
                         #'(lambda ()
                             (when (search-forward-regexp "[\n]+[^ \n]" nil t 1)
                               (cond
                                ((save-excursion (goto-char (line-beginning-position))
                                                 (looking-at "|]"))
                                 (jump))
                                (t (forward-char -1)
                                   (search-backward-regexp "[^\n ]" nil t)
                                   (forward-char)
                                   (point)))))))
                     (jump))
                   (point-max)))))
        (cons start end))))))

(defun brittany-in-comment ()
  "Are we currently in a comment?"
  (save-excursion
    (when (and (= (line-end-position)
                  (point))
               (/= (line-beginning-position) (point)))
      (forward-char -1))
    (and
     (elt (syntax-ppss) 4)
     ;; Pragmas {-# SPECIALIZE .. #-} etc are not to be treated as
     ;; comments, even though they are highlighted as such
     (not (save-excursion (goto-char (line-beginning-position))
                          (looking-at "{-# "))))))

(defun brittany-extra-arguments ()
  "Extra command line arguments for the brittany invocation."
  (append
   (when (boundp 'haskell-language-extensions)
     haskell-language-extensions)
   (when brittany-extra-args
     brittany-extra-args)))

(provide 'brittany)
;;; brittany.el ends here
