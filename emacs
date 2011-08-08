;; We need mouse wheel!
(setq scroll-step 1)

;; We hate the toolbar
(tool-bar-mode 0)

;; Some tweaks
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(mouse-avoidance-mode 'cat-and-mouse)

(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 120))

; Loading color-theme
(add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme-g0sub)
(color-theme-g0sub)

;; I hate auto save and auto backup files scattered all over the file system
;; so we turn them off...
; (setq make-backup-files nil)
; (setq auto-save-default nil)
;; ... or make them all in ONE place
; autosave files (ie #foo#)
(defvar autosave-dir "~/.emacs.d/.auto-save-files/")

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
    (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
      (expand-file-name
        (concat "#%" (buffer-name) "#")))))

; backup files (ie foo~) - backup-directory-alist list contains regexp=>directory mappings
; filenames matching a regexp are backed up in the corresponding directory
(defvar backup-dir "~/.emacs.d/.backup-files/")
(setq backup-directory-alist (list (cons "." backup-dir)))

;; Disable abbrev save to prevent weird annoying
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq save-abbrevs nil)

;; Default encoding for Emacs in terminal
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; My elisp load-path
(add-to-list 'load-path "~/.emacs.d/")

;; This was installed by package-install.el.
;; This provides support for the package system and
;; interfacing with ELPA, the package archive.
;; Move this code earlier if you want to reference
;; packages in your .emacs.
(when
  (load
    (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

; For clojure stuff from technomancy
(add-to-list 'package-archives
             '("technomancy" . "http://repo.technomancy.us/emacs/") t)

;; Load FP stuff: clojure, sbcl, slime

;; Load clojure-mode
(add-to-list 'load-path "~/.emacs.d/clojure-mode/")
(require 'clojure-mode)

;; Load and set up slime
(add-to-list 'load-path "~/.emacs.d/slime/")
(require 'slime)
(add-to-list 'slime-lisp-implementations '(sbcl ("/opt/local/bin/sbcl")))

(eval-after-load "slime"
	'(slime-setup '(slime-fancy slime-banner)))

(add-hook 'lisp-mode-hook
	(lambda ()
	(cond ((not (featurep 'slime))
	  (require 'slime) 
		(normal-mode)))))

(global-set-key (kbd "<f5>") 'slime-selector)

;; Load paredit-mode
(autoload 'paredit-mode "paredit" 
	"Minor mode for pseudo-structurally editing Lisp code." t)
	
(mapc 
	(lambda (mode)
		(let ((hook (intern (concat (symbol-name mode) "-mode-hook"))))
			(add-hook hook (lambda () (paredit-mode +1)))))
	'(emacs-lisp lisp inferior-lisp))

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

;; Keyboard customization for slime
(eval-after-load 'slime
	'(progn
		; * Using built-in functions or paredit-mode functions
		; (define-key slime-mode-map (kbd "[") 'insert-parentheses)
		; (define-key slime-mode-map (kbd "]") 'move-past-close-and-reindent)
		; (define-key slime-mode-map (kbd "(") (lambda () (interactive) (insert "[")))
		; (define-key slime-mode-map (kbd ")") (lambda () (interactive) (insert "]")))
		(define-key slime-mode-map (kbd "[") 'paredit-open-parenthesis)
		(define-key slime-mode-map (kbd "]") 'paredit-close-parenthesis-and-newline)
		(define-key slime-mode-map (kbd "(") 'paredit-open-bracket)
		(define-key slime-mode-map (kbd ")") 'paredit-close-bracket)
		(define-key slime-mode-map [(control ?\-)] (lambda () (interactive) (insert "(")))
		(define-key slime-mode-map [(control ?\=)] (lambda () (interactive) (insert ")")))

		(define-key slime-mode-map (kbd "RET") 'paredit-newline)
		(define-key slime-mode-map (kbd "<return>") 'paredit-newline)
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

;; Syntax highlighting in the slime repl only for swank-clojure
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

;; Load git
(require 'git)

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
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
  '(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Load nxml-mode
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

;; Load markdown-mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

;; Function to run tidy on buffer
(defun tidy-buffer ()
  "Run Tidy HTML parser on current buffer."
  (interactive)
  (if (get-buffer "tidy-errs") (kill-buffer "tidy-errs"))
  (shell-command-on-region (point-min) (point-max)
    "tidy -asxhtml -n -q -i -c -wrap 72 -f /tmp/tidy-errs" t)
  (find-file-other-window "/tmp/tidy-errs")
  (other-window 1)
  (delete-file "/tmp/tidy-errs")
  (message "buffer tidy'ed"))
(global-set-key (kbd "C-x t") 'tidy-buffer)

;; Custom generated
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(case-fold-search nil)
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(rng-nxml-auto-validate-flag nil)
 '(show-paren-mode t)
 '(w3m-default-coding-system (quote utf-8))
 '(w3m-file-coding-system (quote utf-8))
 '(w3m-file-name-coding-system (quote utf-8))
 '(w3m-show-decoded-url (quote (("\\`http://\\(?:[^./?#]+\\.\\)*wikipedia\\.org/" . utf-8) ("\\`http://\\(?:[^./?#]+\\.\\)*nikkei\\.co\\.jp/") ("\\`http://\\(?:[^./?#]+\\.\\)*hatena\\.ne\\.jp/" . utf-8) ("\\`http://\\(?:[^./?#]+\\.\\)*ohmynews\\.co\\.jp/" . utf-8) (t . t)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
