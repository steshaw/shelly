;
; Viper - vi/vim emulation
;
(setq viper-mode t)
(require 'viper)

;
; Aquamacs theme
; 
;
(when (featurep 'aquamacs)
    ;; switch to white on black
    (color-theme-initialize)
    (color-theme-clarity)
    ;; switch to Garamond 36pt
;;    (aquamacs-autoface-mode 0)
;;    (set-frame-font "-apple-garamond-medium-r-normal--36-360-72-72-m-360-iso10646-1")
    ;; switch to fullscreen mode
;;    (aquamacs-toggle-full-frame))
)

;
; Coq
;
(setq auto-mode-alist (cons '("\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;
; Agda
;
;(load-file (let ((coding-system-for-read 'utf-8))
;                (shell-command-to-string "agda-mode locate")))

;
; Solarized
;
(let ((emacs-color-theme-solarized-dir "~/.shelly/local/emacs-color-theme-solarized"))
  (if (featurep 'aquamacs)
    (progn
      (add-to-list 'load-path emacs-color-theme-solarized-dir)
      (require 'color-theme-solarized)
      (color-theme-solarized-light))
    (progn
      (add-to-list 'custom-theme-load-path emacs-color-theme-solarized-dir)
      (load-theme 'solarized-dark t))))

;
; ProofGeneral
;
(if (featurep 'aquamacs)
  (load-file "~/.shelly/apps/ProofGeneral-4.1/generic/proof-site.el"))

; Scala
;
;(add-to-list 'load-path "/path/to/some/directory/scala-mode")
;(require 'scala-mode-auto)

(server-start)
