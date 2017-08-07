(require 'package)

;; Optimize GC while starting emacs
(set 'default-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 10000000)

(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")
        ("gnu-elpa"     . "http://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("melpa"        . 10)
        ("melpa-stable" . 8)
        ("marmalade"    . 5)
        ("gnu-elpa"     . 1)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-ensure t)


;; benchmark emacs initialization
(use-package benchmark-init
  :disabled
  :init (benchmark-init/activate))


;;------------------
;; Themes

(use-package ample-theme
  :disabled
  :init
  (load-theme 'ample t t)
  (enable-theme 'ample))

(use-package tangotango-theme
  :if (not window-system)
  :config
  (load-theme 'tangotango t))

(use-package doom-themes
  :if window-system
  :config
  (load-theme 'doom-one t) ;; M-x all-the-icons-install-fonts
  (doom-themes-neotree-config))


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

(setq-default visible-bell nil
              ring-bell-function #'my/terminal-visible-bell)

;; Enable mouse mode
(xterm-mouse-mode)
(setq-default mouse-wheel-scroll-amount '(3 ((shift) . 1)) ;; three lines at a time
              mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
              mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable line wrap in program-mode
(add-hook 'prog-mode-hook #'toggle-truncate-lines)

;; Column number
(setq column-number-mode t)

;; Disable bold fonts
(set-face-bold 'bold nil)
(mapc (lambda (face)
        (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

;; Set maximum heigth of mini-windows
(setq max-mini-window-height 0.75)

;; Minimum width for splitting windows sensibly
(setq split-width-threshold 80)

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
(setq-default tab-width 4
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
      auto-save-interval 100)            ; number of keystrokes between auto-saves (default: 300)


;; Disable parens highlight delay
(show-paren-mode 0)
(setq show-paren-delay 0)
(show-paren-mode 1)


;;------------------
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

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word."
  (interactive "p")
  (delete-word (- arg)))


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
   ("M-<up>" . (lambda () (interactive) (scroll-down 3)))
   ("M-<down>" . (lambda () (interactive) (scroll-up 3)))
   ("C-<right>" . right-word)
   ("C-<left>" . left-word)
   ("C-f" . forward-word)
   ("C-b" . backward-word)
   ("M-g l" . goto-line)

   ;; Marks
   ("C-c m a" . mark-whole-buffer)
   ("C-c m l" . mark-line)

   ;; Editing
   ("C-w" . backward-delete-word)
   ("M-d" . delete-word)
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
  :commands (package-utils-upgrade-all
             package-utils-upgrade-all-no-fetch
             package-utils-upgrade-by-name
             package-utils-upgrade-by-name-no-fetch
             package-utils-remove-by-name
             package-utils-list-upgrades))

(use-package lacarte
  :bind ("ESC M-x" . lacarte-execute-menu-command))

(use-package flycheck
  :config
  (setq-default flycheck-display-errors-delay 0.5
                flycheck-disabled-checkers '(emacs-lisp-checkdoc)
                flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (global-flycheck-mode))

(use-package eldoc
  :config
  (setq-default eldoc-idle-delay 0.2))

(use-package company
  :bind ("M-/" . company-complete)
  :config
  (setq-default company-minimum-prefix-length 10
                company-tooltip-limit 15
                company-idle-delay .0
                company-echo-delay 0)
  (global-company-mode)
  (load "company-sql.el")
  (use-package company-quickhelp
    :config (company-quickhelp-mode 1)))

(use-package ace-window
  :bind ("M-p" . ace-window))

(use-package neotree
  :bind ("<f8>" . neotree-toggle)
  :config
  (setq-default neo-dont-be-alone t
                neo-window-position 'left
                neo-toggle-window-keep-p t
                neo-theme (if (display-graphic-p) 'arrow 'ascii)
                projectile-switch-project-action 'neotree-projectile-action))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package undo-tree
  :diminish undo-tree-mode
  :bind ("C-x u" . undo-tree-visualize)
  :config
  (setq-default undo-tree-visualizer-diff t
                undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(use-package which-key
  :config (which-key-mode))

(use-package helm
  :demand
  :bind (("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-h a" . helm-apropos)
         ("C-x C-l" . helm-locate))
  :config
  (setq-default helm-M-x-fuzzy-match t
                helm-mode-fuzzy-match t
                helm-candidate-number-limit 100)
  (helm-mode 1)
  (custom-set-faces
   '(helm-ff-directory ((t (:foreground "color-27" :weight bold))))
   '(helm-ff-dotted-directory ((t (:foreground "color-27" :weight bold))))
   '(helm-selection ((t (:foreground "brightwhite" :background "ForestGreen" :distant-foreground "black"))))))

(use-package helm-smex
  :demand
  :bind ("M-X" . helm-smex-major-mode-commands)
  :config
  (setq-default helm-display-header-line nil)
  (global-set-key [remap execute-extended-command] #'helm-smex))

(use-package helm-ls-git
  :commands (helm-ls-git-ls
             helm-browse-project))

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

(use-package parinfer
  :bind ("C-," . parinfer-toggle-mode)
  :init
  (use-package lispy)
  (progn
    (setq parinfer--mode 'indent)
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
            ;; paredit        ; Introduce some paredit commands.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode))
  :config
  (setq smartparens-mode nil))

(use-package aggressive-indent
  :config
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'common-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'scheme-mode-hook #'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode))

;;------------------
;; Yaml

(use-package yaml-mode
  :mode ("\\.yaml$" . yaml-mode))


;;------------------
;; Ensime (Scala)

(use-package ensime
  :commands ensime
  :init
  (setq-default ensime-startup-notification nil
                ensime-startup-snapshot-notification nil))

(use-package sbt-mode
  :mode ("\\.sbt$" . sbt-mode)
  :commands sbt-start sbt-command)

(use-package scala-mode
  :mode ("\\.scala$" . scala-mode))


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
  (add-hook 'ess-mode-hook #'auto-build-tags-hook)
  (add-hook 'ess-mode-hook #'smartparens-mode)
  (add-hook 'inferior-ess-mode-hook #'smartparens-mode)
  (setq split-width-threshold 180
        split-height-threshold 80)
  (setq-default ess-watch-width-threshold 180
                ess-watch-height-threshold 80
                ess-set-style 'RStudio-
                ess-indent-with-fancy-comments nil
                ess-ask-for-ess-directory nil))


;;------------------
;; Haskell

(use-package haskell-mode
  :mode "\\.hs$"
  :config
  ;; (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  ;; (use-package ghc
  ;;   :config
  ;;   (autoload 'ghc-init "ghc" nil t)
  ;;   (autoload 'ghc-debug "ghc" nil t)
  ;;   (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  ;;   (ghc-core-mode))
  (use-package intero
    :config (add-hook 'haskell-mode-hook 'intero-mode))
  (use-package flycheck-haskell
    :init
    (use-package flycheck)
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))


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
  :init
  (setq gofmt-command "goimports")
  :config
  (use-package company-go)
  (use-package go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup))


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
  ;; (use-package paredit
  ;;   :config (add-hook 'clojure-mode-hook #'paredit-mode))
  ;; (use-package rainbow-delimiters
  ;;   :config (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))
  (add-hook 'clojure-mode-hook #'subword-mode)
  :config
  (use-package cider
    :pin melpa-stable
    :init
    (setq-default cider-prompt-for-symbol nil
                  cider-repl-pop-to-buffer-on-connect t
                  cider-show-error-buffer t
                  cider-auto-select-error-buffer t
                  cider-repl-wrap-history t
                  cider-font-lock-dynamically '(macro core function var)
                  cider-repl-use-clojure-font-lock t
                  cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
    (add-hook 'cider-mode-hook #'eldoc-mode))
  (use-package clj-refactor
    :pin melpa-stable
    :config
    (clj-refactor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-r")
    (yas-minor-mode 1)) ; for adding require/use/import statements
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

;; Set GC back to it's original value
(setq gc-cons-threshold default-gc-cons-threshold)

;; ------------------------

(provide 'init)
