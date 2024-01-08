;;; init -- custom settings and functions
;;; Commentary:
;;;   Those are either too specifics or work in progress
;;; Code:

(setq-default
 user-full-name "Tristan Cacqueray"
 user-mail-address "tdecacqu@redhat.com"
 mime-edit-pgp-signers '("EB103DE8B5E69E631C6FF17922B9A05C925CC5D8")
 auth-sources '("~/.authinfo.gpg"))

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

;; (use-package org-alert
;;   :ensure t
;;   :config
;;   (setq-default alert-default-style 'libnotify)
;;   (org-alert-enable)
;; )

(setenv "EDITOR" "emacsclient")

;; disable magit-status pop-up
(setq-default magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;; M-l counsel-git-grep
(global-set-key  (kbd "M-l") 'counsel-git-grep)

;; M-n / M-p to move by paragraph
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; C-ins / M-ins to copy/paste from the system clipboard
(global-set-key (kbd "C-<insert>") 'simpleclip-copy)
(global-set-key (kbd "M-<insert>") 'simpleclip-paste)

;; C-, to yank
(global-set-key (kbd "C-,") 'yank)

;; Unbind C-t for tmux
(global-unset-key (kbd "C-t"))

;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; easier to access undo
(global-set-key (kbd "M-u") 'undo)

;; avy
(global-set-key (kbd "M-g d") 'avy-goto-char-timer)

;; enable standard linux unicode input
(defun read-unicode-char (c1 c2 c3 c4 _trailing_space_ignored)
  "Convert unicode input C1 C2 C3 C4 to the corresponding insert char call."
  (interactive "c\nc\nc\nc\nc")
  (insert-char (string-to-number (format "%c%c%c%c" c1 c2 c3 c4) 16)))
(define-key global-map (kbd "C-S-u") 'read-unicode-char)

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
  (let ((pname
         (if (string= default-directory "~/")
             (getenv "USER") (projectile-project-name)
             )))
    (concat pname "-" suffix)))


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


(require 'mygtd (concat (getenv "HOME") "/.mygtd.el") t)
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
(add-to-list 'eglot-server-programs
             '(rust-ts-mode . start-rust-analyzer))

(setq ispell-program-name "aspell")



(defun xah-get-bounds-of-block ()
  "Return the boundary (START . END) of current block.
Version: 2021-08-12"
  (let ( $p1 $p2 ($blankRegex "\n[ \t]*\n"))
    (save-excursion
      (setq $p1 (if (re-search-backward $blankRegex nil 1)
                    (goto-char (match-end 0))
                  (point)))
      (setq $p2 (if (re-search-forward $blankRegex nil 1)
                    (match-beginning 0)
                  (point))))
    (cons $p1 $p2 )))

(defun xah-get-bounds-of-block-or-region ()
  "If region is active, return its boundary, else same as `xah-get-bounds-of-block'.
Version: 2021-08-12"
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (xah-get-bounds-of-block)))

(defun xah-quote-lines (QuoteL QuoteR Sep)
  "Add quotes/brackets and separator (comma) to lines.
Act on current block or selection.

For example,

 cat
 dog
 cow

becomes

 \"cat\",
 \"dog\",
 \"cow\",

or

 (cat)
 (dog)
 (cow)

In lisp code, QuoteL QuoteR Sep are strings.

URL `http://xahlee.info/emacs/emacs/emacs_quote_lines.html'
Version: 2020-06-26 2023-09-19 2023-10-29"
  (interactive
   (let ((xbrackets
          '(
            "\"double quote\""
            "'single quote'"
            "(paren)"
            "{brace}"
            "[square]"
            "<greater>"
            "`emacs'"
            "`markdown`"
            "~tilde~"
            "=equal="
            "“curly double”"
            "‘curly single’"
            "‹french angle›"
            "«french double angle»"
            "「corner」"
            "none"
            "other"
            ))
         (xcomma '("comma ," "semicolon ;" "none" "other"))
         xbktChoice xsep xsepChoice xquoteL xquoteR)
     (let ((completion-ignore-case t))
       (setq xbktChoice (completing-read "Quote to use:" xbrackets nil t nil nil (car xbrackets)))
       (setq xsepChoice (completing-read "line separator:" xcomma nil t nil nil (car xcomma))))
     (cond
      ((string-equal xbktChoice "none")
       (setq xquoteL "" xquoteR ""))
      ((string-equal xbktChoice "other")
       (let ((xx (read-string "Enter 2 chars, for begin/end quote:")))
         (setq xquoteL (substring xx 0 1)
               xquoteR (substring xx 1 2))))
      (t (setq xquoteL (substring xbktChoice 0 1)
               xquoteR (substring xbktChoice -1))))
     (setq xsep
           (cond
            ((string-equal xsepChoice "comma ,") ",")
            ((string-equal xsepChoice "semicolon ;") ";")
            ((string-equal xsepChoice "none") "")
            ((string-equal xsepChoice "other") (read-string "Enter separator:"))
            (t xsepChoice)))
     (list xquoteL xquoteR xsep)))
  (let (xp1 xp2 (xquoteL QuoteL) (xquoteR QuoteR) (xsep Sep))
    (let ((xbds (xah-get-bounds-of-block-or-region)))
      (setq xp1 (car xbds) xp2 (cdr xbds)))
    (save-excursion
      (save-restriction
        (narrow-to-region xp1 xp2)
        (goto-char (point-min))
        (catch 'EndReached
          (while t
            (skip-chars-forward "\t ")
            (insert xquoteL)
            (end-of-line)
            (insert xquoteR xsep)
            (if (eq (point) (point-max))
                (throw 'EndReached t)
              (forward-char))))))))


(use-package helpful
  :config
;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)
)

(provide 'init)
;;; emacs ends here
