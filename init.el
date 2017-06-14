(require 'package)

(setq package-archives
      '(("gnu-elpa"     . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("marmalade"    . 9)
        ("gnu-elpa"     . 5)
        ("melpa"        . 0)))

(package-initialize)

(defvar my-packages '(better-defaults
                      projectile
                      paredit
                      smartparens
                      aggressive-indent
                      rainbow-delimiters
                      neotree
                      magit
                      undo-tree
                      smex
                      ace-window
                      company
                      company-quickhelp
                      eldoc
                      flycheck
                      lacarte
                      which-key
                      ;; Yaml
                      yaml-mode
                      ;; Clojure
                      clojure-mode
                      cider
                      clj-refactor
                      ;; Scheme
                      geiser
                      ;; Go
                      go-mode
                      company-go
                      go-eldoc
                      ;; Python
                      elpy
                      company-jedi
                      ;; Haskell
                      haskell-mode
                      intero
                      flycheck-haskell
                      ;; Purescript
                      psc-ide
                      ;; R
                      ess
                      ;; Themes
                      ample-theme
                      tangotango-theme
                      ))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))


;;------------------
;; Themes

(load-theme 'tangotango t)
;(enable-theme 'tangotango)

;; Cursor
(setq-default cursor-type '(bar . 2))
(set-cursor-color "#7AA3CC")


;;------------------
;; Custom config

;; Customize bell notification
(defun my/terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell nil ring-bell-function #'my/terminal-visible-bell)

;; Enable mouse mode
(xterm-mouse-mode)
(setq-default mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; three lines at a time
(setq-default mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq-default mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable bold fonts
(set-face-bold 'bold nil)
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

;; Set maximum heigth of mini-windows
(setq max-mini-window-height 0.75)

;; Remove other window on startup
;;(add-hook 'emacs-startup-hook 'delete-other-windows)

;; Trim whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Overwrite selected text
(delete-selection-mode t)

;; Kill scrath buffer
(kill-buffer "*scratch*")

;; Kill whole lines
(setq kill-whole-line t)

;; Enable menu-bar
(global-set-key [f2] 'toggle-menu-bar-mode-from-frame)

;; Follow symbolic links
(setq vc-follow-symlinks t)

;; Keyboard scroll one line at a time
(setq scroll-step 1)

;; Reread a TAGS table without querying
(setq-default tags-revert-without-query t)

;; Backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 100            ; number of keystrokes between auto-saves (default: 300)
      )

;; Disable parens highlight delay
(show-paren-mode 0)
(setq show-paren-delay 0)
(show-paren-mode 1)


;;------------------
;; Lacarte

(require 'lacarte)
(global-set-key (kbd "ESC M-x") 'lacarte-execute-menu-command)


;;------------------
;; Flycheck

(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-display-errors-delay 0.5)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(setq-default flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)


;;------------------
;; Eldoc

(setq-default eldoc-idle-delay 0.2)


;;------------------
;; Company-mode

(global-company-mode)
(company-quickhelp-mode 1)
(global-set-key (kbd "M-/") #'company-complete)
(setq-default company-minimum-prefix-length 3)
(setq-default company-tooltip-limit 15)
(setq-default company-idle-delay .0)
(setq-default company-echo-delay 0)


;;------------------
;; Ace-Window

(global-set-key (kbd "C-x o") 'ace-window)


;;------------------
;; Smex

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;------------------
;; NeoTree

(global-set-key [f8] 'neotree-toggle)
;;(setq-default neo-smart-open t)
(setq-default neo-dont-be-alone t)
(setq-default neo-window-position 'left)
(setq-default neo-toggle-window-keep-p t)
(setq-default neo-theme (if (display-graphic-p) 'arrow 'ascii))
(setq-default projectile-switch-project-action 'neotree-projectile-action) ;; ‘projectile-switch-project’
(neotree-show)


;;------------------
;; Magit

(global-set-key (kbd "C-x g") 'magit-status)


;;------------------
;; Undo-Tree

;(require 'undo-tree)
;(global-undo-tree-mode)


;;------------------
;; Which-Key

(which-key-mode)


;;------------------
;; General Programming

(add-hook 'prog-mode-hook #'smartparens-mode)


;;------------------
;; ESS (R)

(defun auto-build-tags-hook ()
  (add-hook 'after-save-hook
            (lambda ()
              (ess-build-tags-for-directory "." "TAGS"))
            nil t))

(add-hook 'ess-mode-hook #'smartparens-mode)
(add-hook 'ess-mode-hook #'auto-build-tags-hook)
(add-hook 'inferior-ess-mode-hook #'smartparens-mode)

(setq-default ess-set-style 'RStudio-)
(setq-default ess-indent-with-fancy-comments nil)
(setq-default ess-ask-for-ess-directory nil)


;;------------------
;; Haskell

(add-hook 'haskell-mode-hook 'intero-mode)
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

;(eval-after-load 'flycheck
;  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;;------------------
;; Purescript

;; mkdir -p ~/.emacs.d/lib/ && cd ~/.emacs.d/lib/
;; git clone https://github.com/dysinger/purescript-mode.git
;; cd purescript-mode && make purescript-mode-autoloads.el

;; (add-to-list 'load-path "~/.emacs.d/lib/purescript-mode")
;; (require 'purescript-mode-autoloads)
;; (add-to-list 'Info-default-directory-list "~/.emacs.d/lib/purescript-mode")

;; (require 'psc-ide)
;; (add-hook 'purescript-mode-hook
;;   (lambda ()
;;     (psc-ide-mode)
;;     (company-mode)
;;     (flycheck-mode)
;;     (turn-on-purescript-indentation)))


;;------------------
;; Python

(elpy-enable)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)


;;------------------
;; Golang

(add-hook 'go-mode-hook 'go-eldoc-setup)
(setq-default gofmt-command "goimports")


;;------------------
;; Clojure

(defun my-clojure-hook ()
  (clj-refactor-mode 1)
  (smartparens-mode -1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  (cljr-add-keybindings-with-prefix "C-c C-r") ; clj-refactor.el
  )

(add-hook 'clojure-mode-hook #'my-clojure-hook)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'smartparens-mode)

;; Indentation
(require 'clojure-mode)

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

;; Cider
(setq-default cider-prompt-for-symbol nil)
(setq-default cider-repl-pop-to-buffer-on-connect t)
(setq-default cider-show-error-buffer t)
(setq-default cider-auto-select-error-buffer t)
(setq-default cider-repl-wrap-history t)
(setq-default cider-font-lock-dynamically '(macro core function var))
(setq-default cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("\\.cljc$" . clojurec-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))


;;------------------
;; Key bindings

;; Buffers, Windows and Frames
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer. Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-^") 'switch-to-previous-buffer)
(global-set-key (kbd "M-^") 'previous-multiframe-window)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-a") 'ido-write-file)

;; Navigation
(global-set-key (kbd "C-f") 'forward-word)
(global-set-key (kbd "C-b") 'backward-word)

;; Mouse scrolling on terminal
(defun scroll-up-slightly () (interactive) (scroll-up 3))
(defun scroll-down-slightly () (interactive) (scroll-down 3))
(global-set-key (kbd "<mouse-4>") 'scroll-down-slightly)
(global-set-key (kbd "<mouse-5>") 'scroll-up-slightly)

;; Marks
(defun mark-to-beginning-of-buffer ()
  (interactive)
  (push-mark-command nil)
  (beginning-of-buffer)
  (exchange-point-and-mark))

(defun mark-to-end-of-buffer ()
  (interactive)
  (push-mark-command nil)
  (end-of-buffer)
  (exchange-point-and-mark))

(defun mark-line ()
  (interactive)
  (beginning-of-line)
  (push-mark-command nil)
  (end-of-line)
  (exchange-point-and-mark))

(global-set-key (kbd "C-c m a") 'mark-whole-buffer)
(global-set-key (kbd "C-c m l") 'mark-line)
(global-set-key (kbd "C-c m b") 'mark-to-beginning-of-buffer)
(global-set-key (kbd "C-c m e") 'mark-to-end-of-buffer)

;; Editing
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-w") 'kill-region)
(global-set-key (kbd "C-c C-c c") 'insert-char)
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)
(global-set-key (kbd "C-c /") 'comment-line)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-set-key (kbd "C-c C-u") 'upcase-region)
(global-set-key (kbd "C-c C-l") 'downcase-region)
(global-set-key (kbd "C-c C-t") 'capitalize-region)

;; Window Navigation
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Window resizing
(global-unset-key (kbd "C-x C-w"))
(global-set-key (kbd "C-x C-w <left>") (lambda () (interactive) (shrink-window-horizontally 3)))
(global-set-key (kbd "C-x C-w <right>") (lambda () (interactive) (enlarge-window-horizontally 3)))
(global-set-key (kbd "C-x C-w <up>") (lambda () (interactive) (enlarge-window 3)))
(global-set-key (kbd "C-x C-w <down>") (lambda () (interactive) (shrink-window 3)))

;; Smartparens
(define-key smartparens-mode-map (kbd "C-c t") 'sp-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-c o") 'sp-splice-sexp-killing-around)
(define-key smartparens-mode-map (kbd "C-c s") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-c u") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-c k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "M-[ a") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "M-[ b") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "M-[ c") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "M-[ d") 'sp-backward-sexp)


;; ------------------------

;; Load local file
(load "~/emacs.d/local" t)


;; ------------------------

(provide 'init)
