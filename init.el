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
                      evil))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;;------------------
;; Load configs
(require 'smartparens-config)
(require 'clj-refactor)
(require 'auto-complete-config)
(require 'evil)

;;------------------
;; Custom config

(load-theme 'monokai t) ;; Theme
(linum-mode 1) ;; Line numbers

;; Evil
(evil-mode 1)

;; Keyboard scroll one line at a time
(setq scroll-step 1)

;; Auto-Complete
(setq ac-auto-start nil) ;; Not to complete automatically
(setq ac-expand-on-auto-complete nil) ;; Do not auto expand completion
(define-key ac-mode-map (kbd "TAB") 'auto-complete) ;; Completion hotkey
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
