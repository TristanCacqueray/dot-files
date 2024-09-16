;;; .emacs.el --- This file contains my Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2024   Tristan de Cacqueray

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 0.1
;; Author: Tristan de Cacqueray
;; Keywords: emacs config
;; URL: https://github.com/TristanCacqueray/dot-files
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "30"))

;;; Commentary:

;;
;; Below are the settings I like, feel free to report any bad documentations
;; or options I am missing.
;;
;;
;; For the best experience, I recommend building your own Emacs from scratch,
;; one option at a time, checkout these resources:
;; - https://systemcrafters.net/emacs-essentials/absolute-beginners-guide-to-emacs/
;; - https://github.com/purcell/emacs.d
;;
;;
;; This configuration works with the upcoming Emacs-30,
;; Get it with nix:
;; - nix run github:podenv/devenv#emacs
;;
;; Or build it on RHEL:
;; - dnf config-manager --enable codeready-builder-for-rhel-9-x86_64-rpms
;; - dnf install -y texinfo zlib-devel libgccjit-devel ncurses-devel gnutls-devel
;; - git clone --depth 1 -b emacs-30 https://git.savannah.gnu.org/git/emacs.git && cd emacs && ./autogen.sh
;; - ./configure --prefix=/usr/local/emacs --without-all --with-x-toolkit=no --with-native-compilation --with-gpm --with-zlib --enable-link-time-optimization --with-gnutls
;; - make bootstrap && make install

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)


;;;
;;; Core config
;;;

;; Remove decoration, do it early to avoid flicker
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(menu-bar-mode 0)

;; Adjust the defaults to taste
(setq
 ;; Start fullscreen, no about
 inhibit-startup-buffer-menu t
 inhibit-startup-screen t
 inhibit-startup-echo-area-message t
 initial-buffer-choice nil
 initial-scratch-message nil

 ;; Always load the latest file
 load-prefer-newer t
 ;; Ensure file ends with newline (TODO: figure how to temporarly disable this)
 require-final-newline t
 ;; do not create lockfile
 create-lockfiles nil
 ;; Always follow symlinks
 vc-follow-symlinks t

 ;; Select in primary selection, not clipboard
 select-enable-primary t
 select-enable-clipboard nil

 ;; Use 4 spaces for tab
 tab-width 4
 ;; TAB tries to indent the current line or complete the thing at point
 tab-always-indent 'complete

 ;; Keep track of opened files
 recentf-max-saved-items 1024

 ;; Indicate buffer boundary
 indicate-buffer-boundaries 'left

 ;; Scroll one line when cursor moves out of the window
 scroll-step 1
 ;; Scroll up to 100 lines to bring back the cursor on screen
 scroll-conservatively 100

 ;; Do not ring the system bell, but show a visible feedback.
 visible-bell nil

 ;; Paste at cursor position, not at mouse pointer
 mouse-yank-at-point t

 ;; Do not save duplicates in kill-ring
 kill-do-not-save-duplicates t

 ;; Ignore dup entry in interpreter history
 comint-input-ignoredups t

 ;; Increase default column count for fill command
 fill-column 120

 ;; Don't assume that sentences should have two spaces after periods. This ain't a typewriter.
 sentence-end-double-space nil

 ;; Display filepath in window title
 frame-title-format (list '(buffer-file-name "%f" (dired-directory dired-directory "%b")))

 ;; Process performance tuning
 read-process-output-max (* 4 1024 1024)
 process-adaptive-read-buffering nil

 ;; Keep auth info secure
 auth-sources '("~/.authinfo.gpg")

 ;; Use modern spell checker
 ispell-program-name "aspell"

 ;; automatically update dired buffers on revisiting their directory
 dired-auto-revert-buffer t

 ;; Make dired do something intelligent when two directories are shown
 dired-dwim-target t

 ;; save the bookmarks file every time a bookmark is made or deleted
 bookmark-save-flag 1
 )

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Do not save backup in projects, keep them in home
(let ((save-dir (concat user-emacs-directory "saves/")))
  (setq
   auto-save-file-name-transforms `((".*" ,save-dir t))
   backup-directory-alist `((".*" . ,save-dir))))

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;; Default to utf-8 unix encoding
(prefer-coding-system 'utf-8-unix)
;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)


;;;
;;; Core packages and hooks
;;;

;; Save the cursors locations
(save-place-mode)

;; Save the minibuffer history
(savehist-mode)

;; Save recently opened files
(recentf-mode)

;; Auto revert files on change
(global-auto-revert-mode)

;; navigate windows with shift-arrow
(windmove-default-keybindings)

;; show line and column in status line
(line-number-mode)
(column-number-mode)

;; make common symbols pretty
(global-prettify-symbols-mode)

;; visualize matching parens
(show-paren-mode)

;; Always start the server so that sub processes will edit files with the parent emacs
(server-start)
(setenv "EDITOR" "emacsclient")

;; Activate winner mode: use M-x 'winner-undo' to revert a window layout change
(winner-mode)

;; automatic parens pairing
(electric-pair-mode 1)

;; Do not truncate lines by default
(toggle-truncate-lines -1)

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;; Uncomment if auto indentation doesn't work
;; (electric-indent-mode -1)

;; Documentation overlays
(global-eldoc-mode)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode)

;; Delete trailing space on save
;; TODO: figure out how to temporarly disable this feature per buffer when it is annoying
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; make file executable if it has a shebang
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; enable flymake checker for programing file
(add-hook 'prog-mode-hook 'flymake-mode)

;; enable subword
(add-hook 'prog-mode-hook 'subword-mode)

;; ignore code block in spellcheck
(add-to-list 'ispell-skip-region-alist '("^```" . "```$"))

;; remove text property when copying, see https://emacs.stackexchange.com/questions/4187
(defun unpropertize-kill-ring ()
  "Remove text properties for kill ring entries."
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)


;;;
;;; Quality of life packages
;;;

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Setup package manager
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (getenv "NIX_PATH")
  ;; nix user will have to call M-x 'package-refresh-contents' manually.
  (unless package-archive-contents (package-refresh-contents))
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

;; Setup use-package
(require 'use-package)
(setq use-package-verbose t)

;; enable hiding modes from the modeline
(use-package diminish)

;; git porcelaine
(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (magit-auto-revert-mode))

;; access external clipboard, works with wayland
(use-package simpleclip
  :config
  ;; C-ins / M-ins to copy/paste from the system clipboard
  (global-set-key (kbd "C-<insert>") 'simpleclip-copy)
  (global-set-key (kbd "M-<insert>") 'simpleclip-paste)
  (simpleclip-mode))

;; Text completion framework
(use-package company
  :diminish
  :config
  (global-company-mode))

;; VERTical Interactive COmpletion
(use-package vertico
  :custom
  (vertico-count 20)
  :bind (:map vertico-map
              ;; Use page-up/down to scroll vertico buffer, like ivy does by default.
              ("<prior>" . 'vertico-scroll-down)
              ("<next>" . 'vertico-scroll-up))
  :init
  (vertico-mode))

;; Completion style for matching regexps in any order
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  ;; Enable partial completion for file wildcard support
  (completion-category-overrides '((file (styles partial-completion)))))

;; Consulting completing-read, better emacs command
(use-package consult
  :custom
  ;; Disable preview
  (consult-preview-key nil)
  :bind
  (("C-x b" . 'consult-buffer)  ;; Switch buffer, including recentf and bookmarks
   ("M-l" . 'consult-git-grep)
   ("M-y" . 'consult-yank-pop)  ;; Paste by selecting the kill-ring
   ("M-s" . 'consult-line)      ;; Search current buffer, like swiper
   ))

;; Embark let you export a completion list with 'E' amongs other thing.
;; When running git-grep, the export creates a special buffer to browse the results.
(use-package embark
  :demand t ;; fixme: why without :demand, the "Loading package" verbose message is not printed?
  :bind
  (("C-." . embark-act)         ;; Begin the embark process
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :config
  (use-package embark-consult))

;; Better replace with visual feedback
(use-package anzu
  :diminish
  :config
  (global-anzu-mode))

;; Auto formater, TODO: check if reformat works better?
(use-package format-all)

;; Ensure unique buffer names
(progn
  (require 'uniquify) ;; TODO: figure out why (use-package uniquify) fail, it is provided by emacs so that should work..
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/"))

;; solarized theme
(use-package solarized-theme
  :if (display-graphic-p)
  :config
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (load-theme 'solarized-dark-high-contrast t))

 ;; display odd spaces
(use-package whitespace
  :diminish (whitespace-mode)
  :custom
  ;; TODO: tweaks whitespace colors
  (whitespace-style '(face trailing tabs empty space-after-tab space-before-tab tab-mark))
  :config
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package subword
  :diminish subword-mode)

;; use colors to distinguish parens
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; HTML renderer
(use-package shr
  :defer t
  :config
  (setq shr-width 120)
  (setq shr-external-browser 'eww-browse-url)
  (setq shr-color-visible-luminance-min 80))

(use-package all-the-icons
  :if (display-graphic-p))

;; show uncommitted changes in the fringe
(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  ;; TODO: check if this hook is still useful?
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

;; better dired sort filter, e.g. to sort by size
(use-package dired-quick-sort
  :config
  (dired-quick-sort-setup))

;; multiple cursors
(use-package multiple-cursors
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)))

;;;
;;; Custom file format modes
;;;

(require 'eglot)

(use-package haskell-mode
  :config
  ;; ensure run-haskell uses the simplest ghci subprocess
  (setq-default haskell-process-type 'auto)
  ;; (add-hook 'haskell-mode-hook 'ormolu-format-on-save-mode)
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  ;; configure interactive mode
  (require 'haskell-interactive-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)

  (setq
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t
   haskell-process-log t))

(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  )

(use-package dhall-mode
  :mode "\\.dhall\\'"
  :config
  (setq
   dhall-format-arguments (\` ("--ascii"))
   dhall-use-header-line nil))

(use-package gleam-mode
  :disabled ;; Enable by setting :vc, or by finding a release
  :config
  ;; TODO: check if this is still needed
  (add-to-list 'eglot-server-programs '(gleam-mode "gleam" "lsp"))
  (add-to-list 'auto-mode-alist '("\\.gleam$" . gleam-mode)))

(use-package json-mode)

;; For .nix file
(use-package nix-mode)

(use-package org
  :config
  (require 'org-mouse)

  ;; Tell org where are the files
  (setq-default org-directory "~/org/")

  ;; Keep track of when the task was completed
  (setq org-log-done 'time)

  ;; Display image inline
  (setq org-startup-with-inline-images t)

  ;; Ensure shift arrows execute org commands, e.g. change todo state.
  (setq org-support-shift-select nil)

  ;; Insead of "..." show "…" when there's hidden folded content
  ;; Some characters to choose from: …, ⤵, ▼, ↴, ⬎, ⤷, and ⋱
  (setq org-ellipsis "⤵")

  ;; Change state using C-c C-t
  (setq org-use-fast-todo-selection t)

  ;; Disable shift-arrow binding since it used for windmove
  (setq org-support-shift-select 'always)
  (progn
    (add-hook 'org-shiftup-final-hook 'windmove-up)
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right))

  ;; Capture note with `C-c c t'
  (require 'org-capture)
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq-default
   ;; Use any org-agendas file as refile target, only first level
   org-refile-targets '((org-agenda-files :maxlevel . 1))
   ;; Use full outline paths for refile targets
   org-refile-use-outline-path t
   ;; Targets complete directly
   org-outline-path-complete-in-steps nil
   )

  (require 'org-agenda)
  (setq-default
   ;; Start agenda at today
   org-agenda-start-on-weekday nil
   ;; Do not dim blocked tasks
   org-agenda-dim-blocked-tasks nil
   ;; Compact the block agenda view
   org-agenda-compact-blocks t
   )
  )

(use-package markdown-mode)

(use-package purescript-mode
  :config
  (use-package psci)
  (add-hook 'purescript-mode-hook 'inferior-psci-mode))

(use-package rescript-mode)

;; ELisp list function
(use-package dash
  :config
  ;; Syntax highlighting
  (global-dash-fontify-mode))

(use-package go-mode
  :config
  ;; run fmt on save
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package elixir-mode)

(use-package python
  :config
  (add-hook 'python-mode-hook 'format-all-mode)
  (setq python-shell-interpreter "python3"))

(use-package yaml-mode)


;;;
;;; Helpers, to be moved to emacs-toolbox
;;;

(defun generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
     name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))
(defun move-file ()
  "Write this file to a new location, and delete the old one."
  (interactive)
  (let ((old-location (buffer-file-name)))
    (call-interactively #'write-file)
    (when old-location
      (delete-file old-location))))

;; switch to project and load magit right away
(defun project-switch-magit (dir)
  (interactive (list (funcall project-prompter)))
  ;; (project--remember-dir dir)
  (magit-status dir))

(progn
  ;; Display the current date in the modeline
  (setq mode-line-right-align-edge 'right-margin)
  (setq-default mode-line-format
                '("%e" mode-line-front-space
                  (:propertize ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote) display (min-width (5.0)))
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "   "
                  mode-line-position
                  (vc-mode vc-mode)
                  "  "
                  mode-line-modes
                  mode-line-format-right-align ;; push date to the end of the line
                  mode-line-misc-info
                  ))
  (setq-default display-time-format "%Y-%m-%d %a %H:%M")
  (display-time-mode)
  )

(when (display-graphic-p)
  ;; enable standard linux unicode input
  ;; https://emacs.stackexchange.com/a/79645
  (defun read-unicode-char (c1 c2 c3 c4 _trailing_space_ignored)
    "Convert unicode input C1 C2 C3 C4 to the corresponding insert char call."
    (interactive "c\nc\nc\nc\nc")
    (insert-char (string-to-number (format "%c%c%c%c" c1 c2 c3 c4) 16)))
  (define-key global-map (kbd "C-S-u") 'read-unicode-char))


;; Helper to insert latest screenshot at markdown position
(defun get-newest-file-from-dir  (path)
  "Return the latest file in PATH."
  (car (directory-files path 'full nil #'file-newer-than-file-p)))

(defun copy-screenshot-markdown (name)
  "Move the latest screenshot and insert markdown link with NAME."
  (interactive "Mname: ")
  (let* ((infile (expand-file-name (get-newest-file-from-dir "~/Pictures/Screenshots")))
         (outdir (concat (file-name-directory (buffer-file-name)) "/media"))
         (outfile (expand-file-name (concat name ".png") outdir)))
    (unless (file-directory-p outdir)
      (make-directory outdir t))
    (message "copy-screenshot-markdown %s %s" infile outfile)
    (rename-file infile outfile)
    (insert (concat "![" name "](media/" (file-name-nondirectory outfile) ")"))
    (newline)
    (newline)))


(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;; nix powered language servers
(defun start-hls (_mode)
  "Start HLS by trying to wrap in nix develop"
  (cd (projectile-project-root))
  (cond ((file-exists-p "flake.nix")
         (progn
           (message "Running HLS with flake")
           (list "nix" "develop" "--command" "haskell-language-server-wrapper" "--lsp")
           ))
        ;; ((file-exists-p "shell.nix")
        ;;  (progn
        ;;    (message "TODO: handle shell.nix")))
        (t (progn
             (message "Running HLS from host")
             (list "haskell-language-server-wrapper" "--lsp")))
        ))

(defun start-rust-analyzer (_mode)
  "Start rust-analyzer by trying to wrap in nix develop"
  (cd (projectile-project-root))
  (cond ((file-exists-p "flake.nix")
         (progn
           (message "Running rust-analyzer with flake")
           (list "nix" "develop" "--command" "rust-analyzer")
           ))
        ;; ((file-exists-p "shell.nix")
        ;;  (progn
        ;;    (message "TODO: handle shell.nix")))
        (t (progn
             (message "Running rust-analyzer from host")
             (list "rust-analyzer")))
        ))

(add-to-list 'eglot-server-programs '(haskell-mode . start-hls))
(add-to-list 'eglot-server-programs '(rust-mode . start-rust-analyzer))
(add-to-list 'eglot-server-programs '(rust-ts-mode . start-rust-analyzer))


;; Borrowed from https://github.com/purcell/emacs.d
(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  :init-value nil :lighter " Prose" :keymap nil
  (if prose-mode
      (progn
        (when (fboundp 'writeroom-mode)
          (writeroom-mode 1))
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        (setq-local blink-cursor-interval 0.6)
        (setq-local show-trailing-whitespace nil)
        (setq-local line-spacing 0.2)
        (setq-local electric-pair-mode nil)
        (ignore-errors (flyspell-mode 1))
        (visual-line-mode 1))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'blink-cursor-interval)
    (kill-local-variable 'show-trailing-whitespace)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'electric-pair-mode)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (visual-line-mode -1)
    (when (fboundp 'writeroom-mode)
      (writeroom-mode 0))))


;;;
;;; Personal preferences
;;;

;; remove haskell stack
(setq haskell-compiler-type 'cabal
      haskell-process-type 'cabal-repl)

(setq-default
 user-full-name "Tristan Cacqueray"
 user-mail-address "tdecacqu@redhat.com"
 mime-edit-pgp-signers '("EB103DE8B5E69E631C6FF17922B9A05C925CC5D8"))

(if (display-graphic-p)
    ;; In graphic mode, setup a custom font
    (progn
      (ignore-errors (set-frame-font "Iosevka Extended"))
      ;; Use a different font for Emojis
      (ignore-errors (set-fontset-font t '(#x1f300 . #x1fad0) "Noto Color Emoji")))
  ;; Otherwise for terminal, just load a default theme
  (load-theme 'tango-dark t))


;;;
;;; Key bindings and extra customizations
;;;

;; load my toolbox
(let ((my-toolbox "~/src/github.com/TristanCacqueray/emacs-toolbox/"))
  (require 'git-clone (concat my-toolbox "git-clone.el") t)
  (require 'project-shell (concat my-toolbox "project-shell.el") t))

;; load local scripts
(require 'mygtd (concat (getenv "HOME") "/.mygtd.el") t)
(require 'mynotmuch (concat (getenv "HOME") "/.emacs.d/mynotmuch.el") t)

;; Better bindings
(global-set-key (kbd "C-c p p") 'project-switch-magit)
;; Do not ask for permission to kill a buffer
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; M-n / M-p to move by paragraph
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; Unbind C-t for tmux
(global-unset-key (kbd "C-t"))

;; avy
(global-set-key (kbd "M-g d") 'avy-goto-char-timer)

;; Ace navigation
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)

(global-set-key (kbd "C-,") 'yank)
(global-set-key (kbd "M-u") 'undo)
(global-set-key (kbd "M-r") 'undo-redo)

(global-set-key (kbd "<f1>") (lambda () (interactive) (project-shell-history)))
(global-set-key (kbd "<f2>") (lambda () (interactive) (project-shell-history "<2>")))
(global-set-key (kbd "<f3>") 'flymake-goto-next-error)
(global-set-key (kbd "<f5>") 'project-compile)


;;; .emacs.el ends here
