;
; Set up Emacs package system.
;
(require 'package)
(add-to-list 'package-archives
             '("elpa" . "http://elpa.gnu.org/packages"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
;(add-to-list 'package-archives
;             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(package-initialize)

; fetch the list of packages available
(or (file-exists-p package-user-dir)
  (package-refresh-contents))

(defun ensure-packages-installed (&rest packages)
  (mapcar (lambda (package)
    (unless (package-installed-p package)
      (package-install package))) packages))

;
; Fix PATH on MacOS via exec-path-from-shell.
; https://github.com/purcell/exec-path-from-shell
;
(ensure-packages-installed 'exec-path-from-shell)
(message (getenv "PATH"))
(when (memq window-system '(mac ns))
   (exec-path-from-shell-initialize))
(message (getenv "PATH"))

;
; focus-autosave-mode
;
(ensure-packages-installed 'focus-autosave-mode)
(focus-autosave-mode)

;
; Evil
;
(ensure-packages-installed 'evil)
(setq evil-shift-width 2)
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))
(require 'evil)
(evil-mode 1)

;
; Org
;
(ensure-packages-installed 'org)
(setq org-src-fontify-natively t)
;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;
; company-mode
;
(ensure-packages-installed 'company)
(company-mode)
(add-hook 'after-init-hook 'global-company-mode)

;
; Idris
;
(ensure-packages-installed 'idris-mode)

;
; Haskell
;
(ensure-packages-installed 'haskell-mode 'intero)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-type (quote stack-ghci))
 '(package-selected-packages
   (quote
    (solarized-theme projectile intero haskell-mode idris-mode company focus-autosave-mode exec-path-from-shell evil))))
(add-hook 'haskell-mode-hook 'intero-mode)

;
; Agda
;
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(require 'agda-input); for unicode input via latex names like \forall.
(add-hook 'evil-insert-state-entry-hook (lambda () (set-input-method "Agda")))
(add-hook 'evil-insert-state-exit-hook (lambda () (set-input-method nil)))

;
; ProofGeneral
;
;(if (featurep 'aquamacs)
;  (load-file "~/.shelly/apps/ProofGeneral-4.1/generic/proof-site.el"))
;(load-file "/usr/local/Cellar/proof-general/4.2/share/emacs/site-lisp/proof-general/generic/proof-site.el")

;; Open .v files with Proof General's Coq mode
(load "~/.emacs.d/lisp/PG/generic/proof-site")
;; Load company-coq when opening Coq files
(add-hook 'coq-mode-hook #'company-coq-mode)

;
; Projectile
;
(ensure-packages-installed 'projectile)
(projectile-global-mode)


;
; Solarized Theme
;
(ensure-packages-installed 'solarized-theme)
(load-theme 'solarized-dark t)
(load-theme 'solarized-dark t)

;
; Miscelleanous customisations
;

(server-start)

; Put "\n" at the end of the last line like Vim.
(setq require-final-newline t)

; No splash screen!
(setq inhibit-startup-message t)

(setq default-frame-alis
  '((width . 100)
    (height . 45)))

; No tabs!
(setq-default indent-tabs-mode nil)

; Fill to column 100.
(setq-default fill-column 100)

; Don't wrap lines.
(set-default 'truncate-lines t)

(kill-buffer "*scratch*")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo" :foundry "nil" :slant normal :weight normal :height 181 :width normal)))))
