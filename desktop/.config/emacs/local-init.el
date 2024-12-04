;;; local-init -- a getting things done implementation

;; Copyright (C) 2023   Tristan de Cacqueray
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Package-Requires: ((org-ql))

;;; Commentary:
;;;    Use "C-h v" to display variable documentations (or "C-c C-d" for symbol at point with helpful)
;;; Code:

(use-package org-ql)

;; Unfortunately org-mode does not support timestamp with timezone, so this ensure correct format on export.
(setq org-icalendar-timezone "America/New_York")

;; Files
(setq org-report-files '("~/org/projects.org.gpg" "~/org/jira.org"))
(setq tc/inbox "~/org/inbox.org.gpg")
(setq tc/journal "~/org/journal.org.gpg")

;; Look for agenda item in these files
(setq org-agenda-files '("~/org/projects.org.gpg" "~/org/home.org.gpg" "~/org/inbox.org.gpg"))
;; (debug-watch 'org-agenda-files)

(use-package org
  :config
  (setq-default
   ;; Tell org where are the files
   org-directory "~/org/"

   ;; Display image inline
   org-startup-with-inline-images t

   ;; Show headings up to level 2 by default when opening an org files
   org-startup-folded 'content

   ;; Simple TODO sequence, DONE and CANCELLED are terminal
   org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                             (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

   ;; disable org-todo pop-up on C-c C-t
   org-use-fast-todo-selection 'expert

   ;; Mail link description format, %c if from or to when sent by me
   org-email-link-description-format "Email %c (%d): %s"
   )

  ;; Make sure the org is saved
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))


  ;; Custom links
  ;; http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
  (defvar yt-iframe-format
    ;; You may want to change your width and height.
    (concat "<iframe width=\"560\""
            " height=\"315\""
            " src=\"https://www.youtube.com/embed/%s\""
            " frameborder=\"0\""
            " allow=\"accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture\""
            " allowfullscreen></iframe>"))
  (org-add-link-type
   "yt"
   (lambda (handle)
     (browse-url
      (concat "https://www.youtube.com/embed/"
              handle)))
   (lambda (path desc backend)
     (cl-case backend
       (md (format yt-iframe-format   path (or desc "")))
       (html (format yt-iframe-format path (or desc "")))
       (latex (format "\href{%s}{%s}" path (or desc "video"))))))
  )


(setq
 ;; Use any org-agendas file as refile target, only first level
 org-refile-targets '((org-agenda-files :maxlevel . 1))
 ;; Use full outline paths for refile targets
 org-refile-use-outline-path t
 ;; Targets complete directly
 org-outline-path-complete-in-steps nil
 )
;; Ensure refile is accurate.
;; Maybe set this to t when there are too many target
(setq org-refile-use-cache nil)

;; org-capture templates
(setq org-capture-templates
      '(
        ("t" "todo" entry (file tc/inbox)
         "* TODO %?\n/Entered on/ %U")
        ("d" "done" entry (file tc/inbox)
         "* DONE %?\nCLOSED: %U\n")
        ("j" "Journal" entry (file+olp+datetree tc/journal)
         "* %?\n")
        ))

;; Fast shortcut to capture todo directly
(define-key global-map (kbd "<f6>") (lambda () (interactive) (org-capture nil "t")))

(defun tc/org-capture-journal ()
  "Capture a journal item."
  (interactive)
  (org-capture nil "j"))
(define-key global-map (kbd "<f9>") 'tc/org-capture-journal)


;; example agenda config
(defun tc/org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "a"))

(global-set-key (kbd "<f12>") 'tc/org-agenda-show-agenda-and-todo)


(use-package org-agenda
  :config
  ;; gtd settings from [[file:/srv/github.com/rougier/emacs-GTD/GTD.org]]
  (setq-default org-agenda-hide-tags-regexp ".")
  (setq-default org-agenda-prefix-format
                '((agenda . " %i %-12:c%?-12t% s")
                  (todo   . " ")
                  (tags   . " %i %-12:c")
                  (search . " %i %-12:c")))

  (setq org-habit-following-days 7
        org-habit-preceding-days 35
        org-habit-show-habits t)

  ;; Toggle to show all habits ;; todo: make that into a view
  (setq org-habit-show-all-today nil)

  ;; See https://www.labri.fr/perso/nrougier/GTD/index.html
  (setq-default org-agenda-custom-commands
                '(("g" "Get Things Done (GTD)"
                   ((agenda ""
                            ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                             (org-deadline-warning-days 0)))
                    (todo "NEXT"
                          ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                           (org-agenda-prefix-format "  %i %-12:c [%e] ")
                           (org-agenda-overriding-header "\nTasks\n")))
                    (tags-todo "inbox"
                               ((org-agenda-prefix-format "  %?-12t% s")
                                (org-agenda-overriding-header "\nInbox\n")))
                    (tags "CLOSED>=\"<today>\""
                          ((org-agenda-overriding-header "\nCompleted today\n")))))))

  ;; f4 shows the gtd view
  (define-key global-map (kbd "<f4>") (lambda () (interactive) (org-agenda nil "g")))
  )

;; From: https://stackoverflow.com/a/70131908
;; With auto saved disabled
(defun org-archive-done-tasks ()
  "Archive all tasks marked DONE in the file."
  (interactive)
  ;; Disable auto save
  (setq org-archive-subtree-save-file-p nil)
  (unwind-protect
      (mapc (lambda(entry)
              (goto-char entry)
              (org-archive-subtree))
            ;; process the entry in reverse to avoid changes in positioning
            (reverse (org-map-entries (lambda () (point)) "TODO=\"DONE\"" 'file)))
      (setq org-archive-subtree-save-file-p t)))

(use-package org-modern
  :after org
  :custom
  (org-modern-todo-faces
    (quote (("NEXT" :foreground "blue" :weight bold :background "orange"))))
  :config
  (global-org-modern-mode))


;; Show the agenda in the morning
(defun present-agenda ()
  "Show the agenda in full screen."
  (setq org-agenda-restore-windows-after-quit 't)
  (org-agenda nil "g")
  (delete-other-windows))

;; Register a window focus change handler
(add-function :after after-focus-change-function #'tc/show-agenda-after-inactivity)
;; Keep track of the last time emacs got the focus
(setq tc/last-focus (float-time))
(defun tc/show-agenda-after-inactivity ()
  "Display the agenda when Emacs get the focus after being idle for EXPIRY seconds."
  (let ((expiry (* 3600 5)))
    (pcase (frame-focus-state)
      ;; The window got the focus
      (`t (when (> (- (float-time) tc/last-focus) expiry)
            ;; todo: display an animation :)
            (message "Welcome back!")
            (present-agenda)))
      ;; The window lost the focus
      (`nil (setq tc/last-focus (float-time))))))




;; ---8<----8<----8<----8<----8<----8<----8<----8<----8<----8<----8<----8<----
;; TODO - Clean up
;; ---8<----8<----8<----8<----8<----8<----8<----8<----8<----8<----8<----8<----
(defun tc/get-buffer-content ()
  "return buffer or region content."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties 1 (buffer-size))
    )
  )

(when (file-directory-p "/srv/github.com/TristanCacqueray/emacs-toolbox")
  (add-to-list 'load-path "/srv/github.com/TristanCacqueray/emacs-toolbox")
  ;; Update schedule events when the agenda is displayed
  (require 'org-next-event)
  (add-hook 'org-agenda-mode-hook 'org-next-event-render)
  (require 'org-report)
  ;; From anywhere, f6 shows the daily report
  ;; (define-key global-map (kbd "<f6>") 'org-report-daily-show)

  ;; In the daily report calendar view, f6 format the report
  ;; (define-key org-agenda-mode-map (kbd "<f6>") 'org-report-daily)

  )

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

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

(use-package elfeed
  :config
  ;; Mark all YouTube entries
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "youtube\\.com"
                                :add '(video youtube)))
  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
                                :remove 'unread)))
(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "/srv/github.com/TristanCacqueray/midirus.com/content/zettle/feeds.org")))


(defun unzip-to-dir ()
  "Extract zip under Dired cursor to a directory."
  (interactive)
  (let* ((fp (dired-get-filename))
         (ext (file-name-extension fp))
         (dname (substring fp 0 -4)))
    (when (not (string= ext "zip"))
      (error "Not a zip file"))
    (make-directory dname)
    (start-process "unzip" "*unzip-dir*" "unzip" fp "-d" dname)))

(provide 'local-init)
