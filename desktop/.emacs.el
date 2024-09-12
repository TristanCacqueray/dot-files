;;; init -- custom settings and functions
;;; Commentary:
;;;   Those are either too specifics or work in progress
;;; Code:

(setq-default
 user-full-name "Tristan Cacqueray"
 user-mail-address "tdecacqu@redhat.com"
 mime-edit-pgp-signers '("EB103DE8B5E69E631C6FF17922B9A05C925CC5D8")
 auth-sources '("~/.authinfo.gpg"))

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

;; Start the server and setup the EDITOR
(require 'server)
(unless (server-running-p)
  (server-start)
  (setenv "EDITOR" "emacsclient"))

;; disable magit-status pop-up
;; (setq-default magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;; M-l counsel-git-grep
(global-set-key  (kbd "M-l") 'counsel-git-grep)

;; M-n / M-p to move by paragraph
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; C-ins / M-ins to copy/paste from the system clipboard
(global-set-key (kbd "C-<insert>") 'simpleclip-copy)
(global-set-key (kbd "M-<insert>") 'simpleclip-paste)

;; Unbind C-t for tmux
(global-unset-key (kbd "C-t"))

;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; easier to access undo
(global-set-key (kbd "M-u") 'undo)

;; avy
(global-set-key (kbd "M-g d") 'avy-goto-char-timer)

;; Ace navigation
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)

(when (display-graphic-p)
  ;; enable standard linux unicode input
  (defun read-unicode-char (c1 c2 c3 c4 _trailing_space_ignored)
    "Convert unicode input C1 C2 C3 C4 to the corresponding insert char call."
    (interactive "c\nc\nc\nc\nc")
    (insert-char (string-to-number (format "%c%c%c%c" c1 c2 c3 c4) 16)))
  (define-key global-map (kbd "C-S-u") 'read-unicode-char))

;; improve vt rendering
(when (string-equal (getenv "TERM") "xterm-256color")
  (set-background-color "black"))

;; disable eletric indent
(electric-indent-mode -1)

;; Start magit-status when switching project
(require 'projectile)
(setq-default projectile-switch-project-action 'magit-status)

(defun start-worker-process (name program &rest args)
  "Start a process PROGRAM in a buffer NAME with ansi colors."
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
      (switch-to-buffer-other-window *buffer*))))

(defun start-nix-worker-process (name command)
  (start-worker-process name "nix-shell" "--command" command))

(defun get-newest-file-from-dir  (path)
  "Return the latest file in PATH."
  (car (directory-files path 'full nil #'file-newer-than-file-p)))

(defun copy-screenshot-markdown (name)
  "Copy latest screenshot and insert markdown link with NAME."
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




(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

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

;; purescript use flycheck
;; (define-key purescript-mode-map (kbd "<f3>") 'flycheck-next-error)

;; ignore code block in spellcheck
(add-to-list 'ispell-skip-region-alist '("^```" . "```$"))

;; Setup custom packages...
(use-package project-shell
  :load-path "~/src/github.com/TristanCacqueray/emacs-toolbox"
  :config
  (global-set-key (kbd "<f1>") (lambda () (interactive) (project-shell-history "")))
  (global-set-key (kbd "<f2>") (lambda () (interactive) (project-shell-history "<2>"))))

(use-package git-clone
  :load-path "~/src/github.com/TristanCacqueray/emacs-toolbox")

(use-package dired-quick-sort
  :config
  (dired-quick-sort-setup))

(provide 'init)
;;; .emacs.el ends here
