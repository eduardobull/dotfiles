(require 'package)

(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu-elpa"     . "http://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("marmalade"    . 9)
        ("gnu-elpa"     . 5)
         ("melpa"        . 0)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-ensure t)


;;------------------
;; Themes

(use-package ample-theme
  :disabled)

(use-package tangotango-theme
  :config
  (load-theme 'tangotango t))


;;------------------
;; Custom config

;; Theme
(global-hl-line-mode 1)
(set-face-background 'hl-line "#282828")

;; Cursor
(setq-default cursor-type '(bar . 2))
(set-cursor-color "#7AA3CC")

;; Customize bell notification
(defun my/terminal-visible-bell ()
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

;; Column number
(setq column-number-mode t)

;; Disable bold fonts
(set-face-bold 'bold nil)
(mapc (lambda (face)
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

;; Auto revert buffer on file change
(global-auto-revert-mode t)

;; Prevents automatic change of default-directory
(add-hook 'find-file-hook
          (lambda ()
            (setq default-directory command-line-default-directory)))

;; Kill whole lines
(setq kill-whole-line t)

;; Scroll to top or bottom before signaling scroll error
(setq scroll-error-top-bottom t)

;; Enable menu-bar
(global-set-key [f2] 'toggle-menu-bar-mode-from-frame)

;; Follow symbolic links
(setq vc-follow-symlinks t)

;; Keyboard scroll one line at a time
(setq scroll-step 1)

;; Set tab width
(setq-default
 tab-width 4
 indent-tabs-mode nil
 c-basic-offset 4)

;; Reread a TAGS table without querying
(setq-default tags-revert-without-query t)

;; Misc
(setq-default sentence-end-double-space nil)

;; Enable upcase and downcase
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Unset C-z
(global-unset-key (kbd "C-z"))

;; Backups
(setq create-lockfiles nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq make-backup-files nil             ; backup of a file the first time it is saved.
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


;; Functions
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer. Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun mark-line ()
  (interactive)
  (beginning-of-line)
  (push-mark-command nil)
  (end-of-line)
  (exchange-point-and-mark))


;;------------------
;; Packages

(use-package better-defaults)

(use-package bind-key
  :config
  (bind-keys*
   ;; Mouse scrolling on terminal
   ("<mouse-4>" . (lambda () (interactive) (scroll-down 3)))
   ("<mouse-5>" . (lambda () (interactive) (scroll-up 3)))

   ;; Buffers, Windows and Frames
   ("C-^" . switch-to-previous-buffer)
   ("M-RET" . switch-to-previous-buffer)
   ("M-^" . previous-multiframe-window)
   ("C-x C-k" . kill-this-buffer)
   ("C-x C-a" . ido-write-file)

   ;; Navigation
   ("C-<right>" . right-word)
   ("C-<left>" . left-word)
   ("C-f" . forward-word)
   ("C-b" . backward-word)
   ("M-g l" . goto-line)

   ;; Marks
   ("C-c m a" . mark-whole-buffer)
   ("C-c m l" . mark-line)

   ;; Editing
   ("C-w" . backward-kill-word)
   ("C-c C-w" . kill-region)
   ("C-c i c" . insert-char)
   ("C-c /" . comment-line)

   ("C-c C-<SPC>" . rectangle-mark-mode)
   ("C-c r i" . string-insert-rectangle)
   ("C-c r C-w" . kill-rectangle)
   ("C-c r M-w" . copy-rectangle-as-kill)
   ("C-c r C-y" . yank-rectangle)
   ("C-c r DEL" . delete-rectangle)
   ("C-c r d" . delete-rectangle)
   ("C-c r c" . clear-rectangle)

   ("C-c e u" . upcase-region)
   ("C-c e l" . downcase-region)
   ("C-c e c" . capitalize-region)

   ;; Window Navigation
   ("C-x <up>" . windmove-up)
   ("C-x <down>" . windmove-down)
   ("C-x <right>" . windmove-right)
   ("C-x <left>" . windmove-left)
   ("M-z" . zap-up-to-char)

   ;; Window resizing
   ("C-x C-w <left>" . (lambda () (interactive) (shrink-window-horizontally 3)))
   ("C-x C-w <right>" . (lambda () (interactive) (enlarge-window-horizontally 3)))
   ("C-x C-w <up>" . (lambda () (interactive) (enlarge-window 3)))
   ("C-x C-w <down>" . (lambda () (interactive) (shrink-window 3)))))

(use-package projectile
  :bind ("C-x p" . project-find-file)
  :config (projectile-mode))

(use-package package-utils
  :defer t)

(use-package lacarte
  :bind ("ESC M-x" . lacarte-execute-menu-command))

(use-package flycheck
  :init
  (setq-default flycheck-display-errors-delay 0.5)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  :config (global-flycheck-mode))

(use-package eldoc
  :config
  (setq-default eldoc-idle-delay 0.2))

(use-package company
  :bind ("M-/" . company-complete)
  :init
  (setq-default company-minimum-prefix-length 10)
  (setq-default company-tooltip-limit 15)
  (setq-default company-idle-delay .0)
  (setq-default company-echo-delay 0)
  :config
  (global-company-mode)
  (use-package company-quickhelp
    :config (company-quickhelp-mode 1)))

(use-package ace-window
  :bind ("M-p" . ace-window))

(use-package neotree
  :init
  (setq-default neo-dont-be-alone t)
  (setq-default neo-window-position 'left)
  (setq-default neo-toggle-window-keep-p t)
  (setq-default neo-theme (if (display-graphic-p) 'arrow 'ascii))
  (setq-default projectile-switch-project-action 'neotree-projectile-action) ;; ‘projectile-switch-project’
  (global-set-key [f8] 'neotree-toggle)
  :config
  (neotree-show))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package undo-tree
  :diminish undo-tree-mode
  :bind ("C-x u" . undo-tree-visualize)
  :config (global-undo-tree-mode))

(use-package which-key
  :config (which-key-mode))

(use-package helm
  :demand
  :bind (("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-h a" . helm-apropos)
         ("C-x C-l" . helm-locate))
  :init
  (setq-default helm-M-x-fuzzy-match t)
  :config
  (use-package helm-ls-git)
  (helm-mode 1))

(use-package helm-smex
  :bind ("M-X" . helm-smex-major-mode-commands)
  :init
  (setq helm-display-header-line nil)
  (global-set-key [remap execute-extended-command] #'helm-smex))

(use-package smartparens
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  :config
  (define-key smartparens-mode-map (kbd "C-c t") 'sp-transpose-sexp)
  (define-key smartparens-mode-map (kbd "C-c o") 'sp-splice-sexp-killing-around)
  (define-key smartparens-mode-map (kbd "C-c s") 'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "C-c u") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "C-c k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "M-[ a") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "M-[ b") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "M-[ c") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "M-[ d") 'sp-backward-sexp))


;;------------------
;; Yaml

(use-package yaml-mode
  :mode ("\\.yaml$" . yaml-mode))


;;------------------
;; Ensime (Scala)

(use-package ensime
  :pin melpa)

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)

;;------------------
;; ESS (R)

(use-package ess
  :mode ("\\.r$" . ess-mode)
  :init
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
  (setq-default ess-ask-for-ess-directory nil))


;;------------------
;; Haskell

(use-package haskell-mode
  :mode "\\.hs$"
  :init
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  (add-hook 'haskell-mode-hook 'intero-mode)
  :config
  (use-package intero)
  (use-package flycheck-haskell))

;(eval-after-load 'flycheck
;  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;;------------------
;; HTML

(use-package sgml-mode
  :mode ("\\.html$" . html-mode)
  :init
  (add-hook 'html-mode-hook
            (lambda ()
              ;; Default indentation is usually 2 spaces, changing to 4.
              (set (make-local-variable 'sgml-basic-offset) 4))))


;;------------------
;; Python

(use-package elpy
  :mode ("\\.py$" . python-mode)
  :init
  (elpy-enable)
  :config
  (use-package company-jedi)
  (add-to-list 'company-backends 'company-jedi))


;;------------------
;; Golang

(use-package go-mode
  :mode ("\\.go$" . go-mode)
  :config
  (use-package company-go)
  (use-package go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (setq gofmt-command "goimports"))


;;------------------
;; Clojure


(use-package clojure-mode
  :mode (("\\.clj$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljc$" . clojurec-mode)
         ("\\.edn$" . clojure-mode)
         ("\\.boot$" . clojure-mode)
         ("lein-env" . enh-ruby-mode))
  :init
  (use-package paredit
    :config (add-hook 'clojure-mode-hook #'paredit-mode))
  (use-package aggressive-indent
    :config (add-hook 'clojure-mode-hook #'aggressive-indent-mode))
  (use-package rainbow-delimiters
    :config (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))
  (add-hook 'clojure-mode-hook #'subword-mode)
  :config
  (use-package cider
    :init
    (setq-default cider-prompt-for-symbol nil)
    (setq-default cider-repl-pop-to-buffer-on-connect t)
    (setq-default cider-show-error-buffer t)
    (setq-default cider-auto-select-error-buffer t)
    (setq-default cider-repl-wrap-history t)
    (setq-default cider-font-lock-dynamically '(macro core function var))
    (setq-default cider-repl-use-clojure-font-lock t)
    (setq-default cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
    (add-hook 'cider-mode-hook #'eldoc-mode)
    (add-hook 'cider-repl-mode-hook #'smartparens-mode))
  (use-package clj-refactor
    :config
    (clj-refactor-mode 1)
    (smartparens-mode -1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    (cljr-add-keybindings-with-prefix "C-c C-r"))
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))


;; ------------------------

;; Load local file
(load "~/emacs.d/local" t)


;; ------------------------

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode which-key use-package undo-tree tangotango-theme smartparens rainbow-delimiters projectile paredit package-utils neotree magit lacarte intero helm-smex helm-ls-git go-mode git-gutter flycheck-haskell ess ensime elpy company-quickhelp company-jedi clojure-mode better-defaults ample-theme aggressive-indent ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
