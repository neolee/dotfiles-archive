;; My elisp load-path
(add-to-list 'load-path "~/.emacs.d/")

;; We need mouse wheel!
(setq scroll-step 1)

;; We hate the toolbar
(tool-bar-mode 0)

;; Some tweaks
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(mouse-avoidance-mode 'cat-and-mouse)

(add-to-list 'default-frame-alist '(height . 66))
(add-to-list 'default-frame-alist '(width . 140))

;; Loading color theme
; (add-to-list 'load-path "~/.emacs.d/emacs-color-theme")
; (require 'color-theme)
; (add-to-list 'load-path "~/.emacs.d/themes")
; (require 'color-theme-solarized)
; (color-theme-solarized-light)
;; Use built-in color theme engine from Emacs 24
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'solarized-light t)

;; I hate auto save and auto backup files scattered all over the file system
;; so we turn them off...
; (setq make-backup-files nil)
; (setq auto-save-default nil)
;; ... or make them all in ONE place
(defvar autosave-dir "~/.emacs.d/.auto-save-files/")

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
    (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
      (expand-file-name
        (concat "#%" (buffer-name) "#")))))

;; backup files (ie foo~) - backup-directory-alist list contains regexp=>directory mappings
;; filenames matching a regexp are backed up in the corresponding directory
(defvar backup-dir "~/.emacs.d/.backup-files/")
(setq backup-directory-alist (list (cons "." backup-dir)))

;; Disable abbrev save to prevent weird annoying
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq save-abbrevs nil)

;; Default encoding for Emacs in terminal
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Make any instance of Emacs know my PATH well
(setenv "PATH" (shell-command-to-string "echo $PATH"))

;; Additional package repos
(require 'package)
(add-to-list 'package-archives
	'("marmalade" . "http://marmalade-repo.org/packages/"))
; (add-to-list 'package-archives
; 	'("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Forcing show-paren-mode
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; Load FP stuff: clojure, sbcl, slime

;; Load clojure (nrepl & ritz)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar clojure-packages 
	'(clojure-mode
  	nrepl
    nrepl-ritz))
(dolist (p clojure-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defun nrepl-mode-setup ()
  (require 'nrepl-ritz)
	(require 'clojure-mode)
	(let (font-lock-mode)
		(clojure-mode-font-lock-setup)))
(add-hook 'nrepl-interaction-mode-hook 'nrepl-mode-setup)

;; Load and set up slime for SBCL
(add-to-list 'load-path "~/.emacs.d/slime/")
(require 'slime)
(add-to-list 'slime-lisp-implementations '(sbcl ("/usr/local/bin/sbcl")))

(eval-after-load "slime"
  '(slime-setup '(slime-fancy slime-banner)))

(add-hook 'lisp-mode-hook
  (lambda ()
  (cond ((not (featurep 'slime))
    (require 'slime) 
    (normal-mode)))))

(global-set-key (kbd "<f5>") 'slime-selector)

(setq 
  common-lisp-hyperspec-root "file:///Users/neo/Code/Lisp/clhs/HyperSpec/"
  slime-complete-symbol-function 'slime-fuzzy-complete-symbol
  ; slime-startup-animation nil
  slime-enable-evaluate-in-emacs t
  slime-log-events t
  slime-outline-mode-in-events-buffer nil
  slime-repl-return-behaviour :send-only-if-after-complete
  slime-autodoc-use-multiline-p t
  slime-highlight-compiler-notes t
  slime-fuzzy-completion-in-place nil
  )

;; Load paredit-mode
(autoload 'paredit-mode "paredit" 
  "Minor mode for pseudo-structurally editing Lisp code." t)
  
(mapc 
  (lambda (mode)
    (let ((hook (intern (concat (symbol-name mode) "-mode-hook"))))
      (add-hook hook (lambda () (paredit-mode +1)))))
  '(emacs-lisp lisp inferior-lisp clojure nrepl nrepl-interaction))

;; Keyboard customization with paredit
(eval-after-load 'slime 
  '(progn
    (define-key slime-mode-map (kbd "[") 'paredit-open-parenthesis)
    (define-key slime-mode-map (kbd "]") 'paredit-close-parenthesis-and-newline)
    (define-key slime-mode-map (kbd "(") 'paredit-open-bracket)
    (define-key slime-mode-map (kbd ")") 'paredit-close-bracket)
    (define-key slime-mode-map [(control ?\-)] (lambda () (interactive) (insert "(")))
    (define-key slime-mode-map [(control ?\=)] (lambda () (interactive) (insert ")")))

    (define-key slime-mode-map (kbd "C-j") 'newline)
    (define-key slime-mode-map (kbd "\"") 'paredit-doublequote)
    (define-key slime-mode-map (kbd "\\") 'paredit-backslash)
    
    (define-key slime-mode-map (kbd "C-t") 'transpose-sexps)
    (define-key slime-mode-map (kbd "C-M-t") 'transpose-chars)
    
    (define-key slime-mode-map (kbd "C-b") 'backward-sexp)
    (define-key slime-mode-map (kbd "C-M-b") 'backward-char)
    (define-key slime-mode-map (kbd "C-f") 'forward-sexp)
    (define-key slime-mode-map (kbd "C-M-f") 'forward-char)
    (define-key slime-mode-map (kbd "C-n") 'down-list)
    (define-key slime-mode-map (kbd "C-M-n") 'next-line)
    (define-key slime-mode-map (kbd "C-p") 'backward-up-list)
    (define-key slime-mode-map (kbd "C-M-p") 'previous-line)
    
    (define-key slime-mode-map (kbd "C-k") 'kill-sexp)
    (define-key slime-mode-map (kbd "C-M-k") 'paredit-kill)
    
    (define-key slime-mode-map (kbd "C-'") 'paredit-splice-sexp)
    (define-key slime-mode-map (kbd "C-M-l") 'paredit-recentre-on-sexp)
    (define-key slime-mode-map (kbd "C-,") 'paredit-backward-slurp-sexp)
    (define-key slime-mode-map (kbd "C-.") 'paredit-forward-slurp-sexp)
    (define-key slime-mode-map (kbd "C-<") 'paredit-backward-barf-sexp)
    (define-key slime-mode-map (kbd "C->") 'paredit-forward-barf-sexp)
    
    (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
    (define-key slime-mode-map (kbd "C-c TAB") 'slime-complete-form)
    ))

(eval-after-load 'clojure-mode
  '(progn
    (define-key clojure-mode-map (kbd "[") 'paredit-open-parenthesis)
    (define-key clojure-mode-map (kbd "]") 'paredit-close-parenthesis-and-newline)
    (define-key clojure-mode-map (kbd "(") 'paredit-open-bracket)
    (define-key clojure-mode-map (kbd ")") 'paredit-close-bracket)
    (define-key clojure-mode-map [(control ?\-)] (lambda () (interactive) (insert "(")))
    (define-key clojure-mode-map [(control ?\=)] (lambda () (interactive) (insert ")")))

    (define-key clojure-mode-map (kbd "C-j") 'newline)
    (define-key clojure-mode-map (kbd "\"") 'paredit-doublequote)
    (define-key clojure-mode-map (kbd "\\") 'paredit-backslash)
    
    (define-key clojure-mode-map (kbd "C-t") 'transpose-sexps)
    (define-key clojure-mode-map (kbd "C-M-t") 'transpose-chars)
    
    (define-key clojure-mode-map (kbd "C-b") 'backward-sexp)
    (define-key clojure-mode-map (kbd "C-M-b") 'backward-char)
    (define-key clojure-mode-map (kbd "C-f") 'forward-sexp)
    (define-key clojure-mode-map (kbd "C-M-f") 'forward-char)
    (define-key clojure-mode-map (kbd "C-n") 'down-list)
    (define-key clojure-mode-map (kbd "C-M-n") 'next-line)
    (define-key clojure-mode-map (kbd "C-p") 'backward-up-list)
    (define-key clojure-mode-map (kbd "C-M-p") 'previous-line)
    
    (define-key clojure-mode-map (kbd "C-k") 'kill-sexp)
    (define-key clojure-mode-map (kbd "C-M-k") 'paredit-kill)
    
    (define-key clojure-mode-map (kbd "C-'") 'paredit-splice-sexp)
    (define-key clojure-mode-map (kbd "C-M-l") 'paredit-recentre-on-sexp)
    (define-key clojure-mode-map (kbd "C-,") 'paredit-backward-slurp-sexp)
    (define-key clojure-mode-map (kbd "C-.") 'paredit-forward-slurp-sexp)
    (define-key clojure-mode-map (kbd "C-<") 'paredit-backward-barf-sexp)
    (define-key clojure-mode-map (kbd "C->") 'paredit-forward-barf-sexp)
    
    (define-key clojure-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
    (define-key clojure-mode-map (kbd "C-c TAB") 'slime-complete-form)
    ))

;; Load git
; (add-to-list 'load-path "~/.emacs.d/git/")
; (require 'git)
(add-to-list 'load-path "~/.emacs.d/git-emacs")
(require 'git-emacs)

;; And mmm everywhere
(add-to-list 'load-path "~/.emacs.d/mmm-mode/")
(setq mmm-global-mode 'maybe)
(require 'mmm-mode)

;; Enable ido mode by default
(require 'ido)
(ido-mode t)

;; Enable w3m
(add-to-list 'load-path "~/.emacs.d/w3m")
(if window-system
  (require 'w3m-load))

(setq browse-url-browser-function '(
  ("hyperspec" . w3m-browse-url)
  ("HyperSpec" . w3m-browse-url)
  ("." . browse-url-default-macosx-browser)
  ))

;; Load yaml-mode
(add-to-list 'load-path "~/.emacs.d/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
  '(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Load nxml-mode
(load "~/.emacs.d/nxml-mode/rng-auto.el")
(require 'nxml-mode)

(add-to-list 'auto-mode-alist
  (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "wsdl" "wsdd" "xhtml") t) "\\'")
    'nxml-mode))

(unify-8859-on-decoding-mode)

(setq magic-mode-alist
  (cons '("<＼＼?xml " . nxml-mode)
    magic-mode-alist))

(setq nxml-slash-auto-complete-flag t)
(setq nxml-bind-meta-tab-to-complete-flag t)

(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)
