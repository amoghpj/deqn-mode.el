;;; deqn-simulator-interface --- Make interface for simulator settings
;;; Commentary:
;;; TODO (Re)generate python script by detecting changes in deqn file

;;; Code:
(require 'transient)
(require 'deqn-python)
(require 'deqn-parameter-sets)
(require 'dash)

(defun convert-and-do-simulation (&optional args)
  "Call python script and carry out simulation."
  (interactive (list (transient-args 'simulator-options-transient)))
  ;;(deqn-python/translate-model)
  ;; (let ((process "*deqn-python*")))
  (message "%s" args)
  (setq original-fname (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
  
  (shell-command (concat "python " (concat original-fname ".py ") (string-join args " "))))


(define-transient-command simulator-options-transient ()
  "Takes switches while simulating ODE model"
  ["Arguments"
   ("p" "Plot" "--plot")
   ("s" "Save" "--save")
   ("t" "max time" "--tmax=")]
  ["Action"
   ("RET" "Simulate" convert-and-do-simulation)])

(provide 'deqn-simulator-interface)
;;; deqn-simulator-interface ends here

