;; ;; Set up package.el to work with MELPA
(require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;; (package-refresh-contents)

;; ;; Download Evil
;; (unless (package-installed-p 'evil)
;;   (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ;; Download Clojure-mode
;; (unless (package-installed-p 'clojure-mode)
;;   (package-install 'clojure-mode))

(require 'clojure-mode)
(clojure-mode)

(load (expand-file-name "~/.roswell/helper.el"))

(menu-bar-mode 0)
(show-paren-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
(setq-default ispell-local-dictionary "en_US")

(global-display-line-numbers-mode t)
(global-visual-line-mode t)

(add-hook 'emacs-startup-hook 'handle-startup)
(add-hook 'text-mode-hook 'handle-text-hook)
(add-hook 'prog-mode-hook 'handle-prog-hook)

(defun handle-startup ()
  (auto-fill-mode)
  (column-number-mode))
(defun handle-text-hook ()
  (flyspell-mode))
(defun handle-prog-hook ()
  (flyspell-prog-mode))

(defun slime-qlot-exec (directory)
  (interactive (list (read-directory-name "Project directory: ")))
  (slime-start :program "qlot"
               :program-args '("exec" "ros" "-s" "." "run")
               :directory directory
               :name 'qlot
               :env (list (concat "PATH=" (mapconcat 'identity exec-path ":")))))
