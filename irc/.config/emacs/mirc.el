;;; mirc -- my irc!

;; Copyright © 2024 Tristan de Cacqueray
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package erc
  :config
  ;; Setup auth-source
  (setq erc-use-auth-source-for-nickserv-password t)
  ;; Disable password prompts
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-prompt-for-password nil))

(defun erc-libera ()
  (interactive)
  (erc-tls :server "irc.libera.chat"
           :port 6697
           :nick   "tristanC"
           :full-name "Tristan"))

(provide 'mirc)

;;; mirc.el ends here
