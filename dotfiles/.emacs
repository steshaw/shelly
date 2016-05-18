(require 'package)
(add-to-list 'package-archives 
             '("melpa" . "http://melpa.org/packages/"))
;(add-to-list 'package-archives
;             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(package-initialize)

;
; Evil
;
(setq evil-shift-width 2)
(require 'evil)
(evil-mode 1)

;
; Org
;
;(require 'package)
;(setq org-src-fontify-natively t)
;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;
; Agda
;
;(load-file (let ((coding-system-for-read 'utf-8))
;                (shell-command-to-string "agda-mode locate")))
;(add-hook 'evil-insert-state-entry-hook (lambda () (set-input-method "Agda")))
;(add-hook 'evil-insert-state-exit-hook (lambda () (set-input-method nil)))

;
; ProofGeneral
;
;(if (featurep 'aquamacs)
;  (load-file "~/.shelly/apps/ProofGeneral-4.1/generic/proof-site.el"))
(load-file "/usr/local/Cellar/proof-general/4.2/share/emacs/site-lisp/proof-general/generic/proof-site.el")

(server-start)

;(load-theme 'solarized-dark)

(setq default-frame-alis
  '((width . 100)
    (height . 45)))

(custom-set-variables
 '(inhibit-startup-screen t))
