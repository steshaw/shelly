(require 'package)
(package-initialize)
;(add-to-list 'package-archives
;             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)

(server-start)

(setq default-frame-alist 
  '((width . 100)
    (height . 45)))

;
; Org
;
;(require 'package)
;(setq org-src-fontify-natively t)
;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;
; Speedbar
;
;(custom-set-variables
; '(speedbar-show-unknown-files t)
; '(sr-speedbar-auto-refresh t)
; '(sr-speedbar-right-side nil)
;)
;(load-file "~/.emacs.d/sr-speedbar.el")
;(require 'sr-speedbar)
;(sr-speedbar-open)

;
; Aquamacs theme
; 
;
;(when (featurep 'aquamacs)
;    ;; switch to white on black
;    (color-theme-initialize)
;    (color-theme-clarity)
;    ;; switch to Garamond 36pt
;;    (aquamacs-autoface-mode 0)
;;    (set-frame-font "-apple-garamond-medium-r-normal--36-360-72-72-m-360-iso10646-1")
    ;; switch to fullscreen mode
;;    (aquamacs-toggle-full-frame))
;)

;
; Coq
;
(setq auto-mode-alist (cons '("\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;
; Agda
;
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(add-hook 'evil-insert-state-entry-hook (lambda () (set-input-method "Agda")))
(add-hook 'evil-insert-state-exit-hook (lambda () (set-input-method nil)))

;
; Solarized
;
;(let ((emacs-color-theme-solarized-dir "~/.shelly/local/emacs-color-theme-solarized"))
;  (if (featurep 'aquamacs)
;    (progn
;      (add-to-list 'load-path emacs-color-theme-solarized-dir)
;      (require 'color-theme-solarized)
;      (color-theme-solarized-dark))
;    (progn
;      (add-to-list 'custom-theme-load-path emacs-color-theme-solarized-dir)
;      (load-theme 'solarized-dark t))))

;
; ProofGeneral
;
;(if (featurep 'aquamacs)
;  (load-file "~/.shelly/apps/ProofGeneral-4.1/generic/proof-site.el"))

;
; Scala
;

; scala-mode2
;(unless (package-installed-p 'scala-mode2)
;  (package-refresh-contents)
;  (package-install 'scala-mode2))

; ensime
;(add-to-list 'load-path "~/.shelly/local/src/main/elisp/")
;(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;(load-file "~/.emacs.d/confluence.el")
;;(require 'confluence)
;;(setq confluence-url "http://docs.ephox.com/rpc/xmlrpc")

;
; Idris
;
;(unless (package-installed-p 'idris-mode)
;  (package-refresh-contents)
;  (package-install 'idris-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
