;; Setup minimal decorations
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(menu-bar-mode 0)

;; Setup nice theme
(load-theme 'tango-dark)

;; Start fullscreen, no about
(setq
 inhibit-startup-buffer-menu t
 inhibit-startup-screen t
 inhibit-startup-echo-area-message t
 initial-buffer-choice t
 initial-scratch-message nil)

;; Setup a server
(setenv "EDITOR" "emacsclient")
(server-start)

;; Better defaults
(setq
 load-prefer-newer t
 ;; Ensure file ends with newline
 require-final-newline t
 ;; do not create lockfile
 create-lockfiles nil
 ;; Don't use tabs to indent, use 4 spaces instead
 indent-tabs-mode nil
 tab-width 4
 ;; smart tab behavior - indent or complete
 tab-always-indent 'complete
 )

;; Default to utf-8 unix encoding
(prefer-coding-system 'utf-8-unix)
;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; Delete trailing space on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable window layout history
(winner-mode)

;; make file executable if it has a shebang
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; automatic parens pairing
(electric-pair-mode 1)

;; show line and column in status line
(line-number-mode 1)
(column-number-mode 1)
;; make common symbols pretty
(global-prettify-symbols-mode t)
;; visualize matching parens
(show-paren-mode)

;; navigate with shift-arrow
(windmove-default-keybindings)

;; Do not litters
(setq
 ;; Do not save backup in projects, keep them in home
 auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "saves/") t))
 backup-directory-alist `((".*" . ,(concat user-emacs-directory "saves/")))
 )

;; Interactive mode
(use-package comint
  :config
  (setq comint-input-ignoredups t)
  )

;; Save recently opened files
(use-package recentf
  :config
  (setq recentf-max-saved-items 1024)
  (recentf-mode 1))

;; Save the cursors locations
(use-package saveplace
  :config
  (setq save-place-file (concat user-emacs-directory "saveplace"))
  (save-place-mode 1))

;; Save the minibuffer history
(use-package savehist
  :config
  (savehist-mode))

;; Install packages
(setq
 ;; archives to install packages from
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("melpa" . "https://melpa.org/packages/"))
 ;; packages to have installed
 package-selected-packages '(anzu company diminish f s magit smex undo-tree yaml-mode go-mode))

;; Initialize the packages
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(package-install-selected-packages)

;; Ensure unique buffer names
(use-package uniquify
  :config
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  )

;; Completion
(use-package company
  :diminish
  :config
  (global-company-mode))

;; Better history
(use-package undo-tree
  :diminish
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

;; Enable fuzzy matching
(use-package smex
  :config
  (smex-initialize))

;; Better menus
(use-package counsel
  :load-path "~/src/github.com/abo-abo/swiper"
  :diminish
  :config
  (counsel-mode 1)
  )

;; Better search
(use-package swiper
  :load-path "~/src/github.com/abo-abo/swiper"
  :bind (("M-s" . swiper))
  )

;; Better minibuffer completions
(use-package ivy
  :load-path "~/src/github.com/abo-abo/swiper"
  :diminish
  :config
  ;; only show 18 candidates
  (setq ivy-height 18)
  ;; load recenf and bookmarks when using ivy-switch-buffer
  (setq ivy-use-virtual-buffers t)
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; allow out of order inputs
  (setq ivy-re-builders-alist '((t   . ivy--regex-ignore-order)))
  ;; Show full path for virtual buffers
  (setq ivy-virtual-abbreviate 'full)
  ;; Press C-p when you're on the first candidate to select your input
  (setq ivy-use-selectable-prompt t)
  (ivy-mode 1)
  )

;; Better replace with visual feedback
(use-package anzu
  :diminish
  :config
  (global-anzu-mode 1))

;; go lang
(use-package go-mode
  :config
  ;; run fmt on save
  (add-hook 'before-save-hook #'gofmt-before-save))

;; switch to project and load magit right away
(defun project-switch-magit (dir)
  (interactive (list (funcall project-prompter)))
  (project--remember-dir dir)
  (let ((buffer (current-buffer)))
  (unwind-protect
      (progn
        (setq-local project-current-directory-override dir)
	(cd dir)
        (magit-status))
      (with-current-buffer buffer
        (kill-local-variable 'project-current-directory-override)))))

(global-set-key (kbd "C-c p p") 'project-switch-magit)

(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Auto revert files on change
(global-auto-revert-mode t)

;; M-l counsel-git-grep
(global-set-key  (kbd "M-l") 'counsel-git-grep)

;; C-, to yank
(global-set-key (kbd "C-,") 'yank)

;; easier to access undo
(global-set-key (kbd "M-u") 'undo)

;; Setup custom bindings
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f3>") 'next-error)

;; Setup custom packages...
(use-package project-shell
  :load-path "~/src/github.com/TristanCacqueray/emacs-toolbox"
  :config
  (global-set-key (kbd "<f1>") (lambda () (interactive) (project-shell-history)))
  (global-set-key (kbd "<f2>") (lambda () (interactive) (project-shell-history "<2>"))))

(use-package git-clone
  :load-path "~/src/github.com/TristanCacqueray/emacs-toolbox")

;;; .emacs.el ends here
