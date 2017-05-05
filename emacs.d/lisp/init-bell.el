;;; init-bell.el -- Disable ringing of bell.
;;; Commentary:
;;
;; The OS X visible bell is buggy as hell.
;;
;;; Code:


(defvar air-bell-ringing nil
  "Whether my visual bell is currently being rung.

This prevents simultaneously ringing two bells and falling into a race
condition where the bell visualization never clears.")

(setq ring-bell-function (
  lambda ()
  (if (not air-bell-ringing)
    (let* (
      (bg (face-background 'default))
      (fg (face-foreground 'default))
      (reset `(lambda ()
                    ;(set-face-background 'default ,bg)
                    ;(set-face-foreground 'default ,fg)
                    (setq air-bell-ringing nil))))

        ;(set-face-background 'default "NavajoWhite4")
        ;(set-face-foreground 'default "black")
        (setq air-bell-ringing t)

        (run-with-timer 0.05 nil reset)))))

(provide 'init-bell)
;;; init-bell.el ends here
