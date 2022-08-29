;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Neo Lee"
      user-mail-address "neo.lee@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "Iosevka Fixed" :size 16 :weight 'light)
      doom-big-font (font-spec :family "Iosevka Fixed" :size 20 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Iosevka Fixed") ;; inherits the :size from doom-font
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "Iosevka Fixed" :weight 'light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Better defaults
(setq-default
 ;delete-by-moving-to-trash t
 window-combination-resize t
 x-stretch-cursor t
 )

(setq
 undo-limit 80000000
 evil-want-fine-undo t
 auto-save-default t
 scroll-preserve-screen-position 'always
 scroll-margin 2
 )

;; Enable time in the mode-line
(display-time-mode t)

;; Display battery info on laptops
(unless (string-match-p "^Power N/A" (battery))
  (display-battery-mode t))

;; Iterate through CamelCase words
(global-subword-mode t)

;; Split horizontally to right, vertically below the current window.
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Tweak LSP UI
(after! lsp-ui
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover nil
        lsp-log-io nil
        lsp-lens-enable t ; not working properly with ccls!
        lsp-diagnostics-provider :auto
        lsp-enable-symbol-highlighting t
        lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-segments '(symbols)))

;; My keybindings without evil
(map! "C-z" nil)
(setq doom-localleader-alt-key "C-z")

;; smartparens - disabled when lispy mode activated
(map!
 (:after smartparens
  (:map smartparens-mode-map
   "C-M-f"       #'sp-forward-sexp
   "C-M-b"       #'sp-backward-sexp
   "C-M-u"       #'sp-backward-up-sexp
   "C-M-d"       #'sp-down-sexp
   "C-M-p"       #'sp-backward-down-sexp
   "C-M-n"       #'sp-up-sexp
   "C-)"         #'sp-forward-slurp-sexp
   "C-}"         #'sp-forward-barf-sexp
   "C-("         #'sp-backward-slurp-sexp
   "C-{"         #'sp-backward-barf-sexp
   "C-M-s"       #'sp-splice-sexp
   ))
 )

;; buffer-move
(map!
 (:map buffer-move-map
  [C-s-up]       #'buf-move-up
  [C-s-down]     #'buf-move-down
  [C-s-left]     #'buf-move-left
  [C-s-right]    #'buf-move-right))
