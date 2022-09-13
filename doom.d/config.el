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
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

;; (setq +light-theme 'doom-one-light
;;       +dark-theme 'doom-one)
;; (setq +light-theme 'doom-material
;;       +dark-theme 'doom-material-dark)
;; (setq +light-theme 'doom-monokai-pro
;;       +dark-theme 'doom-monokai-pro)
(setq +light-theme 'doom-ayu-light
      +dark-theme 'doom-ayu-mirage)

(defun +apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme +light-theme t))
    ('dark (load-theme +dark-theme t))))

(add-hook! 'ns-system-appearance-change-functions '+apply-theme)

(doom-themes-treemacs-config)
(doom-themes-org-config)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Code/Org/")

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

(global-subword-mode t)

;; doom-modeline tweak
;; Time on modeline
(after! doom-modeline
  (setq display-time-string-forms
        '((propertize (concat " 🕘 " 24-hours ":" minutes))))
  (display-time-mode t)
  ;; Add padding to the right
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "   ")))

;; Battery info on modeline (only on laptops)
(after! doom-modeline
  (let ((battery-str (battery)))
    (unless (or (equal "Battery status not available" battery-str)
                (string-match-p (regexp-quote "unknown") battery-str)
                (string-match-p (regexp-quote "N/A") battery-str))
      (display-battery-mode t))))

;; Customization
(after! doom-modeline
  (setq doom-modeline-bar-width 4
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'auto))

;; lsp-ui
(after! lsp-ui
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover nil
        lsp-log-io nil
        lsp-lens-enable t               ; not working properly with ccls!
        lsp-diagnostics-provider :auto
        lsp-enable-symbol-highlighting t
        lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-segments '(symbols)))

;; zig +lsp
(setq lsp-zig-zls-executable "~/Code/Repo/zls/zig-out/bin/zls")

;; org-mode tweak
(after! org
  (setq org-latex-packages-alist
        '(("" "ctex" t)
          ))
  (setq org-latex-hyperref-template
        "\\hypersetup{linktoc=all,colorlinks=true,urlcolor=blue,linkcolor=blue}")
  )

;; org-babel
(setq org-babel-default-header-args:jupyter-python '((:async . "no")
                                                     (:session . "jp")
                                                     (:kernel . "wop")))

(after! org
  (require 'org-tempo)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (python . t)
                                 (shell . t)
                                 (latex . t)
                                 (sql . t)
                                 (sqlite . t)
                                 (calc . t)
                                 (jupyter . t)))
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                                "xelatex -interaction nonstopmode %f"))
  (setq org-latex-default-packages-alist
        (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
  )

(after! org-src
  (dolist (lang '(python jupyter))
    (cl-pushnew (cons (format "jupyter-%s" lang) lang)
                org-src-lang-modes :key #'car))
  )

;; FIXME the ansi-color text issue in org-babel result block.
;; Source: https://github.com/nnicandro/emacs-jupyter/issues/376
;; Here is the function that jupyter-mime.el relies on,
;; found in an older ansi-color.el in an older emacs distribution.
(defun ansi-color--find-face (codes)
  "Return the face corresponding to CODES."
  ;; Sort the codes in ascending order to guarantee that "bold" comes before
  ;; any of the colors.  This ensures that `ansi-color-bold-is-bright' is
  ;; applied correctly.
  (let (faces bright (codes (sort (copy-sequence codes) #'<)))
    (while codes
      (when-let ((face (ansi-color-get-face-1 (pop codes) bright)))
        (when (and ansi-color-bold-is-bright (eq face 'ansi-color-bold))
          (setq bright t))
        (push face faces)))
    ;; Avoid some long-lived conses in the common case.
    (if (cdr faces)
	(nreverse faces)
      (car faces))))

;; xenops
(use-package! xenops
  :hook (latex-mode . xenops-mode)
  :hook (LaTeX-mode . xenops-mode)
  :hook (org-mode . xenops-mode)
  :defer t
  :config
  (map! :map xenops-mode-map
        :n "RET" #'xenops-dwim)
  (setq xenops-cache-directory (concat doom-cache-dir "xenops/")
        xenops-reveal-on-entry t
        xenops-math-latex-process 'dvisvgm
        xenops-math-image-scale-factor 1.5
        )
  )

;; org-fratog
;; (add-hook! 'org-mode-hook #'org-fragtog-mode)

;; spell-fu
(after! spell-fu
  (defun +spell-fu-register-dictionary (lang)
    "Add `LANG` to spell-fu multi-dict, with a personal dictionary."
    ;; Add the dictionary
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary lang))
    (let ((personal-dict-file (expand-file-name (format "aspell.%s.pws" lang) doom-user-dir)))
      ;; Create an empty personal dictionary if it doesn't exists
      (unless (file-exists-p personal-dict-file) (write-region "" nil personal-dict-file))
      ;; Add the personal dictionary
      (spell-fu-dictionary-add (spell-fu-get-personal-dictionary (format "%s-personal" lang) personal-dict-file))))

  (add-hook! 'spell-fu-mode-hook
    (+spell-fu-register-dictionary "en")
    ))

;; Keybindings without evil
(map! "C-z" nil)
(setq doom-localleader-alt-key "C-z")

;; smartparens (disabled while lispy being activated)
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
 [C-s-up]        #'buf-move-up
 [C-s-down]      #'buf-move-down
 [C-s-left]      #'buf-move-left
 [C-s-right]     #'buf-move-right
 )

;; emacs-jupyter
(after! jupyter
  (map! :after python
        :map python-mode-map
        :localleader
        (:prefix ("j" . "jupyter")
         :desc "Run REPL"          "o" #'jupyter-run-repl
         :desc "Eval funciton"     "f" #'jupyter-eval-defun
         :desc "Eval buffer"       "b" #'jupyter-eval-buffer
         :desc "Eval region"       "r" #'jupyter-eval-region
         :desc "Restart REPL"      "R" #'jupyter-repl-restart-kernel
         :desc "Interrupt REPL"    "i" #'jupyter-repl-interrup-kernel
         :desc "Scratch buffer"    "s" #'jupyter-repl-scratch-buffer
         :desc "Remove overlays"   "O" #'jupyter-eval-remove-overlays
         :desc "Eval string"       "w" #'jupyter-eval-string
         :desc "Inspect at point"  "d" #'jupyter-inspect-at-point
         )))
