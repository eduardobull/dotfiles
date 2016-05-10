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
                      magit
                      undo-tree))

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

;; Misc
(load-theme 'monokai t) ;; Theme
(linum-mode 1) ;; Line numbers
;(ido-ubiquitous-mode 1) ;; Enable ido in all contexts
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Backups
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
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

;; Evil
;(evil-mode 1)

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

;; Undo-Tree
(global-undo-tree-mode)
(defun undo-tree-visualizer-update-linum (&rest args)
  (linum-update undo-tree-visualizer-parent-buffer))
(advice-add 'undo-tree-visualize-undo :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualize-redo :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualize-undo-to-x :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualize-redo-to-x :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualizer-mouse-set :after #'undo-tree-visualizer-update-linum)
(advice-add 'undo-tree-visualizer-set :after #'undo-tree-visualizer-update-linum)


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


;; Cider
;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)
;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)
;; Wrap when navigating history.
(setq cider-repl-wrap-history t)
;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))
