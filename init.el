(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(defvar my-packages '(better-defaults
                      projectile
                      clojure-mode
                      cider
                      smartparens
                      aggressive-indent
                      rainbow-delimiters
                      auto-complete
                      clj-refactor
                      evil
                      neotree
                      magit))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;;------------------
;; Load configs
(require 'smartparens-config)
(require 'clj-refactor)
(require 'auto-complete-config)
(require 'evil)
(require 'neotree)

;;------------------
;; Keybindings
;(global-set-key "M-LEFT" 'forward-word)

;;------------------
;; Custom config

(load-theme 'monokai t) ;; Theme
(linum-mode 1) ;; Line numbers

;; Disable parens highlight delay
(show-paren-mode 0)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Evil
(evil-mode 1)

;; NeoTree
(neotree-show)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t) ;; Every time when the neotree window is opened, let it find current file and jump to node
(setq projectile-switch-project-action 'neotree-projectile-action) ;; ‘projectile-switch-project’
(add-hook 'neotree-mode-hook ;; Work with Evil mode
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Keyboard scroll one line at a time
(setq scroll-step 1)

;; Auto-Complete
(setq ac-auto-start nil) ;; Not to complete automatically
(setq ac-expand-on-auto-complete nil) ;; Do not auto expand completion
(define-key ac-mode-map (kbd "M-/") 'auto-complete) ;; Completion hotkey
(setq ac-ignore-case 'smart) ;; Ignore case if completion target string doesn't include upper characters


;;------------------
;; Hooks

;; Programming
(defun my-prog-hook ()
  ;; Auto-Complete
  (ac-config-default))

(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'prog-mode-hook #'my-prog-hook)

;; Clojure
(defun my-clojure-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'my-clojure-hook)
