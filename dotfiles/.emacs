(viper-mode)

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

; Coq
(setq auto-mode-alist (cons '("\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

(add-to-list 'load-path "~/External/Projects/emacs-color-theme-solarized")
(require 'color-theme-solarized)
(color-theme-solarized-light)

(start-server)
