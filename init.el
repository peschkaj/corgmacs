;;; -*- lexical-binding: t -*-
;;;
;;; init.el - where the corgs begin
;;; corgmacs --- Summary
;;;
;;; Being the emacs configuration of Jeremiah Peschka

;; Copyright (C) 2019 Jeremiah Peschka <jeremiah@legit.biz>
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

;; Set up list of package repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; avoid problems with files newer than their byte-compiled
;; counterparts it's better to have a slower startup than load an
;; outdated and maybe bugged package
(setq load-prefer-newer t)
;; initialize the packages and create the packages list if not exists
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create a new custom-file
;;
;; Having our own custom file means emacs doesn't poop all over this file and
;; constantly generate false changes for version control.
                                        ;(setq custom-file "~/.emacs-custom.el")
(setq custom-file "/dev/null")
(load custom-file 'noerror)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; benchmark-init
;;
;; Uncomment this if emacs is being slow on startup.
;; View important timing stats with M-x benchmark-init/show-durations-tabluation
;;
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(when (not package-archive-contents)
  (package-refresh-contents))

;; install use-package if not exists
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package ibuffer
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up loading from modules directory
;;
;; This code is borrowed from Bodil Stokke's excell ohai-emacs
;; see https://github.com/bodil/ohai-emacs for the original.
;;
;; Figure out the path to our .emacs.d by getting the path part of the
;; current file (`init.el`).
(defvar corgmacs/dotfiles-dir (file-name-directory
                               (or (buffer-file-name) (file-chase-links load-file-name))))

;; Individual modules are stored in a `modules` subdirectory of .emacs.d; it's
;; necessary to add these modules explicitly to the load-path.
(add-to-list 'load-path (concat corgmacs/dotfiles-dir "modules"))

;; TODO add check that this file exists
(let ((corgmacs/local-customizations
      (expand-file-name "corgmacs-custom.el" corgmacs/dotfiles-dir)))
  (if (file-readable-p corgmacs/local-customizations)
      (load-file corgmacs/local-customizations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp settings
;;
;; If you experience tramp hangs, they can fixed by adding the following AT THE
;; VERY END of .zshrc (or .zshrc.local):
;; [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
(use-package tramp
  :init (setq tramp-ssh-controlmaster-options nil))

(defvar corgmacs/notifier-path
  "/usr/bin/notify-send"
  "Notifier program. Can be overriden via ~/.emacs-custom.el")

;; TODO probably need to load0file
;; Check the OS and load any OS specific whatnot...
;; (let ((corgmacs/os-customizations
;;        (cond ((eq system-type 'darwin)
;; 	      "modules/corgmacs-macos.el")
;; 	      "modules/corgmacs-linux.el")))
;;   (if (file-readable-p corgmacs/os-customizations)
;;       (load-file corgmacs/os-customizations)))
(let ((corgmacs/os-customizations
       ;; N.B. This only detects Windows. Everything else is assumed to be some kind of *nix variant
       (cond ((eq system-type 'windows-nt)
              (expand-file-name "cormacs-windows.el" (concat corgmacs/dotfiles-dir "modules")))
             ((eq system-type 'darwin)
              (expand-file-name "corgmacs-macos.el" (concat corgmacs/dotfiles-dir "modules")))
             (t (expand-file-name "corgmacs-linux.el" (concat corgmacs/dotfiles-dir "modules")))
         )))
  (if (file-readable-p corgmacs/os-customizations)
      (load-file corgmacs/os-customizations)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings
;;
;; Includes:
;;  - Sane defaults for warnings
;;  - Time formatted to a 24 hour clock
;;  - Removal of tool, menu, and scroll bars
;;  - Setting up a temporary and autosave location
;;  - Setting up sane defaults around autosaving files
(require 'corgmacs-general)
(require 'corgmacs-editing)
(require 'corgmacs-helm)
(require 'corgmacs-vc)
(require 'corgmacs-code)
(require 'corgmacs-lisp)
(require 'corgmacs-cpp)
(require 'corgmacs-fstar)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell
(require 'corgmacs-haskell)


(require 'corgmacs-org)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pretty symbols
(setq prettify-symbols-unprettify-at-point 'right-edge)

(defconst pragmatapro-prettify-symbols-alist
  (mapcar (lambda (s)
            `(,(car s)
              .
              ,(vconcat
                (apply 'vconcat
                       (make-list
                        (- (length (car s)) 1)
                        (vector (decode-char 'ucs #X0020) '(Br . Bl))))
                (vector (decode-char 'ucs (cadr s))))))
          '(("[ERROR]"   #XE380)
            ("[DEBUG]"   #XE381)
            ("[INFO]"    #XE382)
            ("[WARN]"    #XE383)
            ("[WARNING]" #XE384)
            ("[ERR]"     #XE385)
            ("[FATAL]"   #XE386)
            ("[TRACE]"   #XE387)
            ("[FIXME]"   #XE388)
            ("[TODO]"    #XE389)
            ("[BUG]"     #XE38A)
            ("[NOTE]"    #XE38B)
            ("[HACK]"    #XE38C)
            ("[MARK]"    #XE38D)
            ("!!"        #XE900)
            ("!="        #XE901)
            ("!=="       #XE902)
            ("!!!"       #XE903)
            ("!≡"        #XE904)
            ("!≡≡"       #XE905)
            ("!>"        #XE906)
            ("!=<"       #XE907)
            ("#("        #XE920)
            ("#_"        #XE921)
            ("#{"        #XE922)
            ("#?"        #XE923)
            ("#>"        #XE924)
            ("##"        #XE925)
            ("#_("       #XE926)
            ("%="        #XE930)
            ("%>"        #XE931)
            ("%>%"       #XE932)
            ("%<%"       #XE933)
            ("&%"        #XE940)
            ("&&"        #XE941)
            ("&*"        #XE942)
            ("&+"        #XE943)
            ("&-"        #XE944)
            ("&/"        #XE945)
            ("&="        #XE946)
            ("&&&"       #XE947)
            ("&>"        #XE948)
            ("$>"        #XE955)
            ("***"       #XE960)
            ("*="        #XE961)
            ("*/"        #XE962)
            ("*>"        #XE963)
            ("++"        #XE970)
            ("+++"       #XE971)
            ("+="        #XE972)
            ("+>"        #XE973)
            ("++="       #XE974)
            ("--"        #XE980)
            ("-<"        #XE981)
            ("-<<"       #XE982)
            ("-="        #XE983)
            ("->"        #XE984)
            ("->>"       #XE985)
            ("---"       #XE986)
            ("-->"       #XE987)
            ("-+-"       #XE988)
            ("-\\/"      #XE989)
            ("-|>"       #XE98A)
            ("-<|"       #XE98B)
            (".."        #XE990)
            ("..."       #XE991)
            ("..<"       #XE992)
            (".>"        #XE993)
            (".~"        #XE994)
            (".="        #XE995)
            ("/*"        #XE9A0)
            ("//"        #XE9A1)
            ("/>"        #XE9A2)
            ("/="        #XE9A3)
            ("/=="       #XE9A4)
            ("///"       #XE9A5)
            ("/**"       #XE9A6)
            (":::"       #XE9AF)
            ("::"        #XE9B0)
            (":="        #XE9B1)
            (":≡"        #XE9B2)
            (":>"        #XE9B3)
            (":=>"       #XE9B4)
            (":("        #XE9B5)
            (":-("       #XE9B6)
            (":)"        #XE9B7)
            (":-)"       #XE9B8)
            (":/"        #XE9B9)
            (":\\"       #XE9BA)
            (":3"        #XE9BB)
            (":D"        #XE9BC)
            (":P"        #XE9BD)
            (":>:"       #XE9BE)
            (":<:"       #XE9BF)
            ("<$>"       #XE9C0)
            ("<*"        #XE9C1)
            ("<*>"       #XE9C2)
            ("<+>"       #XE9C3)
            ("<-"        #XE9C4)
            ("<<"        #XE9C5)
            ("<<<"       #XE9C6)
            ("<<="       #XE9C7)
            ("<="        #XE9C8)
            ("<=>"       #XE9C9)
            ("<>"        #XE9CA)
            ("<|>"       #XE9CB)
            ("<<-"       #XE9CC)
            ("<|"        #XE9CD)
            ("<=<"       #XE9CE)
            ("<~"        #XE9CF)
            ("<~~"       #XE9D0)
            ("<<~"       #XE9D1)
            ("<$"        #XE9D2)
            ("<+"        #XE9D3)
            ("<!>"       #XE9D4)
            ("<@>"       #XE9D5)
            ("<#>"       #XE9D6)
            ("<%>"       #XE9D7)
            ("<^>"       #XE9D8)
            ("<&>"       #XE9D9)
            ("<?>"       #XE9DA)
            ("<.>"       #XE9DB)
            ("</>"       #XE9DC)
            ("<\\>"      #XE9DD)
            ("<\">"      #XE9DE)
            ("<:>"       #XE9DF)
            ("<~>"       #XE9E0)
            ("<**>"      #XE9E1)
            ("<<^"       #XE9E2)
            ("<!"        #XE9E3)
            ("<@"        #XE9E4)
            ("<#"        #XE9E5)
            ("<%"        #XE9E6)
            ("<^"        #XE9E7)
            ("<&"        #XE9E8)
            ("<?"        #XE9E9)
            ("<."        #XE9EA)
            ("</"        #XE9EB)
            ("<\\"       #XE9EC)
            ("<\""       #XE9ED)
            ("<:"        #XE9EE)
            ("<->"       #XE9EF)
            ("<!--"      #XE9F0)
            ("<--"       #XE9F1)
            ("<~<"       #XE9F2)
            ("<==>"      #XE9F3)
            ("<|-"       #XE9F4)
            ("<<|"       #XE9F5)
            ("<-<"       #XE9F7)
            ("<-->"      #XE9F8)
            ("<<=="      #XE9F9)
            ("<=="       #XE9FA)
            ("==<"       #XEA00)
            ("=="        #XEA01)
            ("==="       #XEA02)
            ("==>"       #XEA03)
            ("=>"        #XEA04)
            ("=~"        #XEA05)
            ("=>>"       #XEA06)
            ("=/="       #XEA07)
            ("=~="       #XEA08)
            ("==>>"      #XEA09)
            ("≡≡"        #XEA10)
            ("≡≡≡"       #XEA11)
            ("≡:≡"       #XEA12)
            (">-"        #XEA20)
            (">="        #XEA21)
            (">>"        #XEA22)
            (">>-"       #XEA23)
            (">=="       #XEA24)
            (">>>"       #XEA25)
            (">=>"       #XEA26)
            (">>^"       #XEA27)
            (">>|"       #XEA28)
            (">!="       #XEA29)
            (">->"       #XEA2A)
            ("??"        #XEA40)
            ("?~"        #XEA41)
            ("?="        #XEA42)
            ("?>"        #XEA43)
            ("???"       #XEA44)
            ("?."        #XEA45)
            ("^="        #XEA48)
            ("^."        #XEA49)
            ("^?"        #XEA4A)
            ("^.."       #XEA4B)
            ("^<<"       #XEA4C)
            ("^>>"       #XEA4D)
            ("^>"        #XEA4E)
            ("\\\\"      #XEA50)
            ("\\>"       #XEA51)
            ("\\/-"      #XEA52)
            ("@>"        #XEA57)
            ("|="        #XEA60)
            ("||"        #XEA61)
            ("|>"        #XEA62)
            ("|||"       #XEA63)
            ("|+|"       #XEA64)
            ("|->"       #XEA65)
            ("|-->"      #XEA66)
            ("|=>"       #XEA67)
            ("|==>"      #XEA68)
            ("|>-"       #XEA69)
            ("|<<"       #XEA6A)
            ("||>"       #XEA6B)
            ("|>>"       #XEA6C)
            ("|-"        #XEA6D)
            ("||-"       #XEA6E)
            ("~="        #XEA70)
            ("~>"        #XEA71)
            ("~~>"       #XEA72)
            ("~>>"       #XEA73)
            ("[["        #XEA80)
            ("]]"        #XEA81)
            ("\">"       #XEA90)
            ("_|_"       #XEA97)
            )))

(defun add-pragmatapro-prettify-symbols-alist ()
  (dolist (alias pragmatapro-prettify-symbols-alist)
    (push alias prettify-symbols-alist)))

(defun fp-prettify-symbols ()
  "Make the letters pretty!"
  (mapc (lambda (pair) (push pair prettify-symbols-alist))
        ;; Need a way to make this work with comments as well.
        '(
          ("\\" . ?λ)
          ("*" . ?⋅)
          ;; ("forall" . ?∀)
          ;; ("forAll" . ?∀)
          ;; ("all"    . ?∀)
          ;; ("exists" . ?∃)
          ;; ("undefined" . ?⊥)
          ;; ("elem" . ?∈)
          ;; ("flip elem" . ?∋)
          ;; ("notElem" . ?∉)
          ;; ("flip notElem" . ?∌)
          ;; ("member" . ?∈)
          ;; ("notMember" . ?∉)
          ;; ("union" . ?⋃)
          ;; ("intersection" . ?⋂)
          ;; ("isSubsetOf" . ?⊆)
          ;; ("isProperSubsetOf" . ?⊂)
          (" . " . (? (Br . Bl) ?◦ (Br . Bl) ? ))
          ;; ("/" . ?÷)
          ;; ("div" . ?÷)
          ;; ("quot" . ?÷)
          )))


(when (display-graphic-p)
  (add-hook 'haskell-mode-hook  #'fp-prettify-symbols)
  (add-hook 'prog-mode-hook     #'add-pragmatapro-prettify-symbols-alist)
  (add-hook 'c-mode-hook        #'add-pragmatapro-prettify-symbols-alist)
  (toggle-frame-maximized)
  (global-prettify-symbols-mode +1))

(add-hook 'haskell-mode-hook  #'fp-prettify-symbols)
(add-hook 'prog-mode-hook     #'add-pragmatapro-prettify-symbols-alist)
(add-hook 'c-mode-hook        #'add-pragmatapro-prettify-symbols-alist)
(toggle-frame-maximized)
(global-prettify-symbols-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown
(use-package markdown-mode
  :ensure t
  :config (autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyfreq
;;
;; tracks how often commands are used without a shortcut
;; view usage with keyfreq-show
(use-package keyfreq
  :ensure t
  :blackout t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configuration for spaceline + spaceline-all-the-icons
(use-package all-the-icons
  :ensure t
  )

(use-package nyan-mode
  :ensure t
  :config (nyan-mode))

(use-package git-gutter
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'git-gutter-mode))

(use-package fancy-battery
  :ensure t
  :config (fancy-battery-mode))

(use-package spaceline
  :ensure t)

(use-package spaceline-config
  :ensure spaceline
  :after spaceline
  :config
  (setq powerline-default-separator 'bar))

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config

  (setq spaceline-all-the-icons-separator-type 'none)
  ;(setq spaceline-all-the-icons-separator-type 'arrow)
  (setq spaceline-all-the-icons-icon-set-modified 'circle)

  (spaceline-toggle-all-the-icons-vc-icon-on)
  (spaceline-toggle-all-the-icons-mode-icon-on)
  (spaceline-toggle-all-the-icons-vc-status-on)
  (spaceline-toggle-all-the-icons-text-scale-on)
  ;; nyan takes care of this
  (spaceline-toggle-all-the-icons-hud-off)
  ;; p sure that the time is leading to more power draw
  ;(spaceline-toggle-all-the-icons-time-off)
  ;; git-ahead is currently disabled because it starts spamming messages that
  ;; 'Buffer " *temp*" has a running processes' but nobody actually cares.
  ;; (spaceline-all-the-icons--setup-git-ahead)
  ;; (spaceline-toggle-all-the-icons-git-ahead-on)

  (spaceline-toggle-all-the-icons-git-status-on)
  (spaceline-all-the-icons-theme))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-term
(use-package multi-term
  :ensure t
  :config
  (add-hook 'term-mode-hook (lambda ()
                              (define-key term-raw-map (kbd "C-y") 'term-paste)))
  (setq multi-term-program "/usr/local/bin/zsh"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing tools
(use-package writegood-mode
  :ensure t
  :config (add-hook 'text-mode-hook 'writegood-mode))

(use-package langtool
  :ensure t
  :config
  (progn (setq langtool-language-tool-server-jar "/usr/local/Cellar/languagetool/4.4/libexec/languagetool.jar"
               langtool-server-user-arguments '("-p" "8082")
               langtool-bin "/usr/local/bin/languagetool")
         ;; TODO move these to :bind
         (global-set-key "\C-x4w" 'langtool-check)
         (global-set-key "\C-x4W" 'langtool-check-done)
         (global-set-key "\C-x4l" 'langtool-switch-default-language)
         (global-set-key "\C-x44" 'langtool-show-message-at-point)
         (global-set-key "\C-x4c" 'langtool-correct-buffer)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load theme as the very last activity
;(load-theme 'tangotango t)
;(load-theme 'srcery t)
(load-theme 'flatland t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global blackouts
(blackout 'auto-revert-mode)



;; TODO: Remove this, it should be the user's choice if emacs is a daemon or not
(server-start)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
