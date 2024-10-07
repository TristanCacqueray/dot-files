;;; media-player -- setup a media player

;; Copyright © 2024 Tristan de Cacqueray
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package ready-player
  :load-path "/srv/github.com/xenodium/ready-player"
  :custom
  ;; keep the buffer separator to avoid confusion
  (ready-player-hide-modeline nil)
  ;; disable video window, I just want music. TODO: implement a behavior toggle
  (ready-player-open-playback-commands
   '(("mpv" "--audio-display=no" "--video=no" "--input-ipc-server=<<socket>>")))
  ;; easy access to my library
  (ready-player-my-media-collection-location "~/Music/")
  :config
  ;; Recognize video files as audio.
  (setq ready-player-supported-audio
        (seq-remove (lambda (m) (string= m "mov"))
                    (append ready-player-supported-audio
                            ready-player-supported-video)))
  ;; Don't recognize any file as video.
  (setq ready-player-supported-video nil)
  (ready-player-mode +1))

(provide 'media-player)

;;; media-player.el ends here
