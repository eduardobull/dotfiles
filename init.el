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
                      smartparens
                      aggressive-indent
                      rainbow-delimiters
                      neotree
                      magit
                      undo-tree
                      smex
                      ace-window
                      company
                      flycheck
                      ;; Languages
                      clojure-mode
                      cider
                      clj-refactor
                      go-mode
                      company-go
                      go-eldoc
                      elpy
                      company-jedi
                      ;; Themes
                      ample-theme
                      ))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))


;;------------------
;; Themes

(load-theme 'ample t t)
(enable-theme 'ample)

;; Cursor
(setq-default cursor-type '(bar . 2))
(set-cursor-color "#7AA3CC")


;;------------------
;; Custom config

;; Enable menu-bar
(global-set-key [f2] 'toggle-menu-bar-mode-from-frame)


;; Follow symbolic links
(setq vc-follow-symlinks t)

;; Keyboard scroll one line at a time
(setq scroll-step 1)

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
;; Key bindings

;; window navigation
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key "\M-z" 'zap-up-to-char)

;; Window resizing
;(global-set-key (kbd "S-<left>") 'shrink-window-horizontally)
;(global-set-key (kbd "S-<right>") 'enlarge-window-horizontally)
;(global-set-key (kbd "S-<up>") 'enlarge-window)
;(global-set-key (kbd "S-<dow>") 'shrink-window)
;(global-set-key (kbd "M-[ d") 'shrink-window-horizontally)  ;;KiTTY
;(global-set-key (kbd "M-[ c") 'enlarge-window-horizontally) ;;KiTTY
;(global-set-key (kbd "M-[ a") 'enlarge-window) ;;KiTTY
;(global-set-key (kbd "M-[ b") 'shrink-window) ;;KiTTY

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


;;------------------
;; Flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)


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
(setq neo-dont-be-alone t)
(setq neo-window-position 'left)
(setq neo-smart-open t) ;; Every time when the neotree window is opened, let it find current file and jump to node
(setq projectile-switch-project-action 'neotree-projectile-action) ;; ‘projectile-switch-project’
(setq neo-theme (if (display-graphic-p) 'arrow 'nerd))
(neotree-show)


;;------------------
;; Magit

(global-set-key (kbd "C-x g") 'magit-status)


;;------------------
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
;; General Programming

(add-hook 'prog-mode-hook #'smartparens-mode)


;;------------------
;; Python

(elpy-enable)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)


;;------------------
;; Golang

(add-hook 'go-mode-hook 'go-eldoc-setup)
(setq gofmt-command "goimports")


;;------------------
;; Clojure

(defun my-clojure-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-r"))

;;(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'my-clojure-hook)

;; Cider
(setq cider-prompt-for-symbol nil)
(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-wrap-history t)
(setq cider-font-lock-dynamically '(macro core function var))
(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("\\.cljc$" . clojurec-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; Company-mode
(global-company-mode)
(global-set-key (kbd "M-/") #'company-complete)
(setq company-minimum-prefix-length 3)
(setq company-tooltip-limit 15)
(setq company-idle-delay .0)
(setq company-echo-delay 0)

(provide 'init)
;;; init.el ends here
