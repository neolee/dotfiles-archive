;; We need mouse wheel!
(mouse-wheel-mode t)
(setq scroll-step 1)

;; Some tweaks
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(mouse-avoidance-mode 'cat-and-mouse)

;; Loading color-theme
; (add-to-list 'load-path "~/.emacs.d/color-theme")

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

;; Default encoding for Emacs in terminal
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; My elisp load-path
(add-to-list 'load-path "~/.emacs.d/")

;; And mmm everywhere
(add-to-list 'load-path "~/.emacs.d/mmm-mode/")
(setq mmm-global-mode 'maybe)
(require 'mmm-mode)
; (setq mmm-global-mode t)
; (require 'mmm-auto)

;; Disable abbrev save to prevent weird annoying
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq save-abbrevs nil)

;; Enable ido mode by default
(require 'ido)
(ido-mode t)

;; Load slime and swank-clojure
(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime-autoloads)

(setq slime-lisp-implementations `(
	(sbcl ("/opt/local/bin/sbcl"))
	; (clisp ("/opt/local/bin/clisp"))
	))

(add-hook 'lisp-mode-hook
	(lambda ()
	(cond ((not (featurep 'slime))
	  (require 'slime) 
		(normal-mode)))))

(eval-after-load "slime"
	'(slime-setup '(slime-fancy slime-banner)))

(global-set-key (kbd "<f5>") 'slime-selector)

; Load clojure stuff
(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(require 'clojure-auto)

(add-to-list 'load-path "~/.emacs.d/swank-clojure")
(require 'swank-clojure-autoload)       
(swank-clojure-config
	(setq swank-clojure-jar-path (expand-file-name "~/Code/Clojure/clojure/clojure.jar"))
  (setq swank-clojure-extra-classpaths (list (expand-file-name "~/Code/Clojure/clojure-contrib/src"))))

;; Load paredit-mode
(autoload 'paredit-mode "paredit" 
	"Minor mode for pseudo-structurally editing Lisp code." t)
	
(mapc 
	(lambda (mode)
		(let ((hook (intern (concat (symbol-name mode) "-mode-hook"))))
			(add-hook hook (lambda () (paredit-mode +1)))))
	'(emacs-lisp lisp inferior-lisp))

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
		(define-key slime-mode-map [(return)] 'paredit-newline)
		(define-key slime-mode-map [(control ?\=)] (lambda () (interactive) (insert "(")))
		(define-key slime-mode-map [(control ?\\)] (lambda () (interactive) (insert ")")))
		(define-key slime-mode-map (kbd "C-t") 'transpose-sexps)
		(define-key slime-mode-map (kbd "C-M-t") 'transpose-chars)
		(define-key slime-mode-map (kbd "C-b") 'backward-sexp)
		(define-key slime-mode-map (kbd "C-M-b") 'backward-char)
		(define-key slime-mode-map (kbd "C-f") 'forward-sexp)
		(define-key slime-mode-map (kbd "C-M-f") 'forward-char)
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

;; Load emacs-ruby, emacs-rails and deps
(add-to-list 'load-path "~/.emacs.d/ruby-mode")

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(add-hook 'ruby-mode-hook
	(lambda()
		(add-hook 'local-write-file-hooks
			'(lambda()
				(save-excursion
					(untabify (point-min) (point-max))
					(delete-trailing-whitespace)
				)))
		(imenu-add-to-menubar "IMENU")
		; Not sure if this line is 100% right but it works!
		(define-key ruby-mode-map "\C-m" 'newline-and-indent)
		(set (make-local-variable 'indent-tabs-mode) 'nil)
		(set (make-local-variable 'tab-width) 2)
		(require 'ruby-electric)
		(ruby-electric-mode t)
	))

(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

(add-hook 'ruby-mode-hook
	'(lambda ()
		(inf-ruby-keys)))

(autoload 'rubydb "rubydb3x" "Ruby debugger" t)

(require 'find-recursive) 
(require 'snippet)

(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)

(add-to-list 'load-path "~/.emacs.d/emacs-rails")
(require 'rails)

; ri-ruby
(add-to-list 'load-path "~/.emacs.d/ri-emacs")
(setq ri-ruby-script (expand-file-name "~/.emacs.d/ri-emacs/ri-emacs.rb"))
(autoload 'ri "ri-ruby.el" nil t)
(add-hook 'ruby-mode-hook (lambda ()
	(local-set-key "\C-cd" 'ri)
	(local-set-key "\C-cf" 'ri-ruby-complete-symbol)
	(local-set-key "\C-ca" 'ri-ruby-show-args)
))

; rcodetool
(add-to-list 'load-path "~/.emacs.d/rcodetool")
(require 'rcodetools)
(setq rct-find-tag-if-available nil)
(defun make-ruby-scratch-buffer ()
  (with-current-buffer (get-buffer-create "*ruby scratch*")
    (ruby-mode)
    (current-buffer)))
(defun ruby-scratch ()
  (interactive)
  (pop-to-buffer (make-ruby-scratch-buffer)))
(defun ruby-mode-hook-rcodetools ()
	(define-key ruby-mode-map (kbd "C-c C-c") 'ruby-scratch))
	(define-key ruby-mode-map (kbd "C-c C-d") 'xmp)
(add-hook 'ruby-mode-hook 'ruby-mode-hook-rcodetools)

; Set mmm for erb
(eval-after-load "mmm-mode"
  '(progn
  	(mmm-add-classes
    	'((eruby :submode ruby-mode :front "<%[#=]?" :back "-?%>"
               :match-face (("<%#" . mmm-comment-submode-face)
                            ("<%=" . mmm-output-submode-face)
                            ("<%"  . mmm-code-submode-face))
               :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
                        (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
                        (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @)))))
    (mmm-add-mode-ext-class 'rhtml-mode "\\.html\\.erb$" 'eruby)
    (mmm-add-mode-ext-class 'rhtml-mode "\\.rhtml$" 'eruby)
    (mmm-add-mode-ext-class 'yaml-mode "\\.yml$" 'eruby)))

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

;; Load cedet
(load-file "~/.emacs.d/cedet/common/cedet.el")

; Enabling various SEMANTIC minor modes. See semantic/INSTALL for more ideas.
; Select one of the following:
; * This enables the database and idle reparse engines
; (semantic-load-enable-minimum-features)
; * This enables some tools useful for coding, such as summary mode
;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)
; * This enables even more coding tools such as the nascent intellisense mode
;   decoration mode, and stickyfunc mode (plus regular code helpers)
; (semantic-load-enable-guady-code-helpers)
; * This turns on which-func support (Plus all other code helpers)
; (semantic-load-enable-excessive-code-helpers)
; * This turns on modes that aid in grammar writing and semantic tool
;   development.  It does not enable any other features such as code
;   helpers above.
; (semantic-load-enable-semantic-debugging-helpers)

;; Load elib
(add-to-list 'load-path "~/.emacs.d/elib")
;; Load jdee
(add-to-list 'load-path "~/.emacs.d/jde/lisp")
(require 'jde)

;; Load ecb
(add-to-list 'load-path "~/.emacs.d/ecb")
(require 'ecb)

;; Launch ecb by default
; (ecb-activate)

;; Custom generated
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search nil)
 '(ecb-layout-name "oasis")
 '(ecb-options-version "2.33beta2")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-show-sources-in-directories-buffer (quote ("left7" "left13" "left14" "left15" "single")))
 '(ecb-source-path (quote ("~/Code")))
 '(ecb-tip-of-the-day nil)
 '(ecb-tree-expand-symbol-before t)
 '(ecb-wget-setup (quote ("Please_add_wget_to_your_path_or_set_the_fullpath_to_wget" . other)))
 '(jde-global-classpath (quote ("/Users/neo/Code/Clojure/clojure/classes" "/Users/neo/Code/Clojure/clojure/src/clj" ".")))
 '(jde-jdk (quote ("1.5.0")))
 '(jde-jdk-registry (quote (("1.4.2" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.4") ("1.5.0" . "/System/Library/Frameworks/JavaVM.framework") ("1.6.0" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.6"))))
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(rng-nxml-auto-validate-flag nil)
 '(semanticdb-default-save-directory "~/.emacs.d/semanticdb")
 '(show-paren-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
