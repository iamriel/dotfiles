;;; init-python.el -- EPC configuration
;;; Commentary:
;;
;; It's just a matter of code.
;;
;;; Code:
(require 'epc)
(when noninteractive
  (load "subr")
  (load "byte-run"))
(eval-when-compile (require 'cl))

(message "Start EPC")

(defvar my-epc-server-py
  (expand-file-name "my-server.py"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defvar my-epc (epc:start-epc (or (getenv "PYTHON") "python")
                              (list my-epc-server-py)))

(message "Start request")

(deferred:$
  (epc:call-deferred my-epc 'echo '(10))
  (deferred:nextc it
    (lambda (x) (message "Return : %S" x))))


(message "Return : %S" (epc:call-sync my-epc 'echo '(10 40)))

(loop for i from 1 to 5
      do (deferred:$
           (epc:call-deferred my-epc 'echo (list i))
           (deferred:nextc it
             (lambda (x) (message "Return : %S" x)))))

(message "Return : %S"
         (epc:sync my-epc (epc:query-methods-deferred my-epc)))
(provide 'init-epc)
;;; init-epc.el ends here
