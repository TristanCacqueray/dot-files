;;; init -- custom settings and functions
;;; Commentary:
;;;   Those are either too specifics or work in progress
;;; Code:

(setq-default
 user-full-name "Tristan Cacqueray"
 user-mail-address "tdecacqu@redhat.com"
 mime-edit-pgp-signers '("EB103DE8B5E69E631C6FF17922B9A05C925CC5D8"))

(setenv "EDITOR" "emacsclient")

;; M-l insert lambda
(global-set-key  (kbd "M-l") (lambda () (interactive) (insert "λ")))

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
                                   "-XOverloadedLists"
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

;; reason lsp configuration
(when (and (require 'lsp-mode nil t) (require 'reason-mode nil t))
  (progn
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "reason-language-server")
                      :major-modes '(reason-mode)
                      :notification-handlers (ht ("client/registerCapability" 'ignore))
                      :priority 1
                      :server-id 'reason-ls))
    (add-hook 'reason-mode-hook 'lsp-mode)
    ))
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
  (let ((home (getenv "HOME"))
        (inf (parse-git-url url)))
    (when (null (url-host inf))
      (error "Invalid url: %s" url))
    (concat
     home "/src/" (url-host inf)
     (replace-regexp-in-string
      "/r/" "/"
      (replace-regexp-in-string
       ".git$" "" (url-filename inf))))))

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
    (dired d)))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun make-pull-request-against-my-fork ()
  (interactive)
  (message "Forking output %s" (process-lines "hub" "fork"))
  (mpr-make-pull-request "origin" "TristanCacqueray" "main" t))

(defun remove-window-decoration ()
  (interactive)
  (let ((frame-name (frame-parameter (selected-frame) 'name)))
    (call-process-shell-command (concat
                                 "xprop -name \""
                                 frame-name
                                 "\" -f _MOTIF_WM_HINTS 32c -set _MOTIF_WM_HINTS '0x2, 0x0, 0x0, 0x0, 0x0'"))))

(provide 'init)
;;; emacs ends here
