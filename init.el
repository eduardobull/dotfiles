(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize)

(defvar my-packages '(better-defaults
                      projectile
                      clojure-mode
                      cider
                      smartparens
                      aggressive-indent
                      rainbow-delimiters
                      auto-complete
                      clj-refactor))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;;------------------
;; Load configs
(require 'smartparens-config)
(require 'clj-refactor)
(require 'auto-complete-config)

;;-----------------
;; Custom config
(load-theme 'monokai t)

;; keyboard scroll one line at a time
(setq scroll-step 1) 

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

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'my-clojure-hook)
(put 'upcase-region 'disabled nil)
