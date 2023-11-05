;;; init -- custom settings and functions
;;; Commentary:
;;;   Those are either too specifics or work in progress
;;; Code:

(setq-default
 user-full-name "Tristan Cacqueray"
 user-mail-address "tdecacqu@redhat.com"
 mime-edit-pgp-signers '("EB103DE8B5E69E631C6FF17922B9A05C925CC5D8")
 auth-sources '("~/.authinfo.gpg"))

(setenv "EDITOR" "emacsclient")

;; should make windmove key work in org-mode
(setq org-replace-disputed-keys t)

;; M-l insert lambda
(global-set-key  (kbd "M-l") (lambda () (interactive) (insert "λ")))

;; M-n / M-p to move by paragraph
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; C-ins / M-ins to copy/paste from the system clipboard
(global-set-key (kbd "C-<insert>") 'simpleclip-copy)
(global-set-key (kbd "M-<insert>") 'simpleclip-paste)

;; Unbind C-t for tmux
(global-unset-key (kbd "C-t"))

;; improve vt rendering
(when (string-equal (getenv "TERM") "xterm-256color")
  (set-background-color "black"))

;; disable eletric indent
(electric-indent-mode -1)

;; enable quality of life ghc extensions
(setq haskell-language-extensions (list
                                   "-XBangPatterns"
                                   "-XBinaryLiterals"
                                   "-XDataKinds"
                                   "-XDeriveGeneric"
                                   "-XDerivingStrategies"
                                   "-XExplicitForAll"
                                   "-XFlexibleInstances"
                                   "-XGeneralizedNewtypeDeriving"
                                   "-XHexFloatLiterals"
                                   "-XImportQualifiedPost"
                                   "-XLambdaCase"
                                   "-XMultiWayIf"
                                   "-XNamedFieldPuns"
                                   "-XNamedWildCards"
                                   "-XNumDecimals"
                                   "-XNumericUnderscores"
                                   "-XOverloadedStrings"
                                   "-XPostfixOperators"
                                   "-XRecordWildCards"
                                   "-XScopedTypeVariables"
                                   "-XStandaloneDeriving"
                                   "-XTupleSections"
                                   "-XTypeOperators"
                                   ))
(setq flycheck-ghc-args haskell-language-extensions)


;; Start magit-status when switching project
(require 'projectile)
(setq-default
 projectile-switch-project-action (quote magit-status))
(global-set-key (kbd "<f5>") (quote projectile-compile-project))

;; Ace navigation
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)

;; Start a process in a buffer with ansi colors
(defun start-worker-process (name program &rest args)
  (let ((buffer-name (concat "*" name "*")))
    (message "Starting %s %s" buffer-name program)
    (let ((*buffer* (get-buffer-create buffer-name)))
      (if (get-buffer-process *buffer*)
          (message "Process already running!")
        (with-current-buffer *buffer*
          (cd (projectile-project-root))
          (let ((*proc* (apply 'start-process name buffer-name program args)))
            (ansi-color-for-comint-mode-on)
            (comint-mode)
            (set-process-filter *proc* 'comint-output-filter)
            )))
      (switch-to-buffer-other-window *buffer*)
      )))

(defun start-nix-worker-process (name command)
  (start-worker-process name "nix-shell" "--command" command))

;; Create project name based id
(defun project-id (suffix)
  (concat (projectile-project-name) "-" suffix))

;; function to start shell
(defun pshell ()
  (interactive)
  (let ((*buffer* (get-buffer-create (concat "*" (project-id "shell") "*"))))
    (if (get-buffer-process *buffer*)
        (switch-to-buffer-other-window *buffer*)
      (shell *buffer*))
    ))
(global-set-key (kbd "<f1>") 'pshell)

;; haskell helper
(defun project-hoogle ()
  (interactive)
  (start-nix-worker-process (project-id "hoogle") "hoogle server -p 8080 --local --haskell"))

;; reason helper
(defun pnpm-start ()
  (interactive)
  (start-worker-process (project-id "pnpm-start") "pnpm" "run" "start"))

(defun pnpm-serve ()
  (interactive)
  (start-worker-process (project-id "pnpm-serve") "sh" "-c" "pnpm run build && pnpm run serve"))

(defun get-newest-file-from-dir  (path)
  (car (directory-files path 'full nil #'file-newer-than-file-p)))

(defun copy-screenshot-markdown (name)
  "Copy latest screenshot and insert markdown link"
  (interactive "Mname: ")
  (let* ((infile (expand-file-name (get-newest-file-from-dir "~/Pictures/Screenshots")))
        (outdir (concat (file-name-directory (buffer-file-name)) "/media"))
        (outfile (expand-file-name (concat name ".png") outdir)))
    (unless (file-directory-p outdir)
      (make-directory outdir t))
    (message "copy-screenshot-markdown %s %s" infile outfile)
    (rename-file infile outfile)
    (insert (concat (concat "![" name "](media/" (file-name-nondirectory outfile)) ")")))
  (newline)
  (newline))

;; better comint
(defun turn-on-comint-history ()
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (mkdir (concat user-emacs-directory "comint-history") t)
      (setq comint-input-ring-file-name
            (expand-file-name (format (concat user-emacs-directory "comint-history/%s")
                                      (replace-regexp-in-string "\*" "" (buffer-name (current-buffer))))))
      (message "buffer history: %s" comint-input-ring-file-name)
      (comint-read-input-ring t)
      (add-hook 'kill-buffer-hook 'comint-write-history-on-exit nil :local))))
(defun comint-write-history-on-exit ()
  ;; debug
  (message "Writting %s" comint-input-ring-file-name)
  (comint-write-input-ring))
(use-package comint
  :defer t
  :config
  ;; Increase comint buffer size.
  (setq comint-buffer-maximum-size 32768)
  ;; Save per buffer history
  (add-hook 'shell-mode-hook 'turn-on-comint-history)
  ;; Save buffer history on exit
  (defun comint-write-input-ring-all-buffers ()
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (comint-write-history-on-exit)))
          (buffer-list)))
  (add-hook 'kill-emacs-hook 'comint-write-input-ring-all-buffers)
  )

;; a git clone helper
(defun parse-git-url (url)
  (url-generic-parse-url url))

(defun giturl-to-dir (url)
  "Convert a git URL to a local path."
  (let ((inf (parse-git-url url)))
    (when (null (url-host inf))
      (error "Invalid url: %s" url))
    (concat
     "/srv/" (url-host inf)
     (replace-regexp-in-string
      " " ""
      (replace-regexp-in-string
       "/r/" "/"
       (replace-regexp-in-string
        ".git$" "" (url-filename inf)))))))

(defun git-clone-url (url dir)
  (message "clonning %s to %s" url dir)
  (mkdir dir t)
  (call-process "git" nil (get-buffer-create "*git-clone-log*") nil "clone" url dir))

(defun f-git? (path)
  (f-directory? (concat path "/.git")))

(defun git-clone (url)
  "Create directory, clone and open project"
  (interactive "Murl: ")
  (let ((d (giturl-to-dir url)))
    (unless (f-git? d)
      (git-clone-url url d))
    ;; todo: check if clone process succeeded
    (dired d)))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun remove-window-decoration ()
  (interactive)
  (let ((frame-name (frame-parameter (selected-frame) 'name)))
    (call-process-shell-command (concat
                                 "xprop -name \""
                                 frame-name
                                 "\" -f _MOTIF_WM_HINTS 32c -set _MOTIF_WM_HINTS '0x2, 0x0, 0x0, 0x0, 0x0'"))))


(require 'mynotmuch (concat (getenv "HOME") "/.emacs.d/mynotmuch.el") t)

;; start hls with nix wrapper
;; (setq lsp-haskell-server-wrapper-function
;;       (lambda (argv)
;;         (append (list "nix-shell" "-I" (lsp-haskell--get-root) "--command")
;;                 (list (mapconcat 'identity argv " ")))
;;         ))

;; (setq lsp-haskell-server-wrapper-function nil)

(setq ormolu-extra-args
      '("--ghc-opt" "-XTypeApplications"
        "--ghc-opt" "-XImportQualifiedPost"
        "--ghc-opt" "-XPatternSynonyms"))


;; Switch to fourmolu
(defun my/fourmolu ()
  (setq lsp-haskell-formatting-provider "fourmolu")
  (setq ormolu-format-on-save-mode nil)
  (remove-hook 'haskell-mode-hook 'ormolu-format-on-save-mode)
  (add-hook 'haskell-mode-hook 'format-all-mode)
  't
  )
(my/fourmolu)
;; (setq lsp-haskell-formatting-provider "ormolu")

;; remove haskell stack
(setq haskell-compiler-type 'cabal)
(setq haskell-process-type 'cabal-repl)
;; (setq flycheck-checkers (remove 'haskell-stack-ghc flycheck-checkers))

;; (require 'gleam-mode)
(add-to-list 'auto-mode-alist '("\\.gleam$" . gleam-mode))

;; org mode journaling
(setq org-capture-templates '(
                              ("t" "todo" entry (file "~/org/refile.org.gpg")
                               "* TODO %? %a\n%U\n")
                              ("j" "Journal" entry (file+olp+datetree "~/org/journal.org.gpg")
                               "* %?\n")
                              ))

(defun tc/org-capture-journal ()
  (interactive)
  "Capture a journal item"
  (org-capture nil "j"))
(define-key global-map (kbd "C-9") 'tc/org-capture-journal)
(defun org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "a"))
(use-package org-agenda
  :bind ("<f12>"   . org-agenda-show-agenda-and-todo)
  :config
  (setq
   ;; Start agenda at today
   org-agenda-start-on-weekday nil
   ;; Look for agenda item in every org files
   org-agenda-files '("~/org")
   ;; Match encrypted files too
   org-agenda-file-regexp "\\`[^.].*\\.org\\(.gpg\\)?\\'"
   ;; Do not dim blocked tasks
   org-agenda-dim-blocked-tasks nil
   ;; Compact the block agenda view
   org-agenda-compact-blocks t
   ;; Customize view
   org-agenda-custom-commands
   (quote (("N" "Notes" tags "NOTE"
            ((org-agenda-overriding-header "Notes")
             (org-tags-match-list-sublevels t)))
           ("h" "Habits" tags-todo "STYLE=\"habit\""
            ((org-agenda-overriding-header "Habits")
             (org-agenda-sorting-strategy
              '(todo-state-down effort-up category-keep))))
           (" " "Agenda"
            ((agenda "" nil)
             (tags "REFILE"
                   ((org-agenda-overriding-header "Tasks to Refile")
                    (org-tags-match-list-sublevels nil)))))))))

(setq rust-format-on-save t)

(setq undo-tree-auto-save-history nil)

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

(require 'eglot)
(add-to-list 'eglot-server-programs
             '(haskell-mode . start-hls))
(add-to-list 'eglot-server-programs
             '(gleam-mode "gleam" "lsp"))
(add-to-list 'eglot-server-programs
             '(rust-mode . start-rust-analyzer))



(setq load-path (cons "/srv/github.com/leanprover/lean4-mode" load-path))
(require 'lean4-mode)

(setq ispell-program-name "aspell")

(provide 'init)
;;; emacs ends here
