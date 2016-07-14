;
; Set up Emacs package system.
;
(require 'package)
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

(ensure-packages-installed
  'exec-path-from-shell
  'evil
  'haskell-mode
  'idris-mode
  'org
  'solarized-theme
  )

;
; Fix PATH on MacOS via exec-path-from-shell.
; https://github.com/purcell/exec-path-from-shell
;
(message (getenv "PATH"))
(when (memq window-system '(mac ns))
   (exec-path-from-shell-initialize))
(message (getenv "PATH"))

;
; Evil
;
(setq evil-shift-width 2)
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))
(require 'evil)
(evil-mode 1)

;
; Org
;
;(require 'package)
;(setq org-src-fontify-natively t)
;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;
; Haskell
;
(custom-set-variables '(haskell-process-type 'stack-ghci))

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
; Miscelleanous customisations
;

(server-start)

(load-theme 'solarized-light t)

; Put "\n" at the end of the last line like Vim.
(setq require-final-newline t)

; No splash screen!
(setq inhibit-startup-message t)

(setq default-frame-alis
  '((width . 100)
    (height . 45)))

; No tabs!
(setq-default indent-tabs-mode nil)

; Fill to column 80.
(setq-default fill-column 80)

; Don't wrap lines.
(set-default 'truncate-lines t)

(kill-buffer "*scratch*")
