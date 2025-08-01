;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Steven Shaw"
      user-mail-address "steven@steshaw.org")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "Iosevka Term JBMS" :size 15 :weight 'normal))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(defconst light-theme 'doom-one-light)
;(defconst light-theme 'doom-acario-light)

;(defconst dark-theme 'doom-dracula)
;(defconst dark-theme 'doom-material-dark)
;(defconst dark-theme 'doom-monokai-octagon)
;(defconst dark-theme 'doom-moonlight)
;(defconst dark-theme 'doom-nord)
;(defconst dark-theme 'doom-one)
;(defconst dark-theme 'doom-palenight)
;(defconst dark-theme 'doom-rouge)
;(defconst dark-theme 'doom-snazzy)
(defconst dark-theme 'doom-tokyo-night)
;(defconst dark-theme 'doom-vibrant)
;(defconst dark-theme 'doom-wilmersdorf)

(message "SHELLY_DARK_MODE = %s" (getenv "SHELLY_DARK_MODE"))
(defun set-colour-scheme (dark-or-light)
  (setq doom-theme
        (if (equal dark-or-light "dark")
            dark-theme
            light-theme)))
(defun update-colour-scheme ()
  (set-colour-scheme (getenv "SHELLY_DARK_MODE")))
(update-colour-scheme)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Code/steshaw/notes/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq doom-localleader-key ",")

; Use Agda mode for literal Agda file with Markdown i.e. *.lagda.md.
(setq auto-mode-alist
   (append
     '(("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))


;; Reload files that have changed on disk.
(global-auto-revert-mode t)

;; Save all buffers when Emacs loses focus.
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;; Auto-save all buffers regularly.
;(setq auto-save-visited-mode t)
;(setq auto-save-visited-interval 1)
;(auto-save-visited-mode +1)

;; AsciiDoc
;(add-hook! adoc-mode
; (add-to-list 'auto-mode-alist '("\\.adoc" . adoc-mode))
; (flyspell-mode t))

(setq auto-mode-alist
   (append
     '(("\\.adoc\\'" . adoc-mode))
     auto-mode-alist))

; Org
(setq org-time-stamp-rounding-minutes '(0 15))
(setq org-duration-format (quote h:mm))
; Try to stop weirld org parsing bug.
(setq org-element-use-cache nil)
