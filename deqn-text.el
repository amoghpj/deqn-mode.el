;;; deqn-text.el --- deqn's text export, useful for custom parsing

;;; Commentary:

;;; Code:

(require 'cl)
(require 'deqn-utils)

(defun deqn-write-text ()
    "Export current file to plain text tab separated files.
This will generate three files, containing the equations, the parameters,
and the initial conditions respectively."
    (interactive)
    (deqn-parse-and-validate-buffer)
    ;; Write equations
    
    (setq-local deqn-text/equations "# equations\n")
    (dolist (eqn deqn-equations)
      (setq-local deqn-text/equations (concat deqn-text/equations
                                              (substring (nth 0 eqn) 1) "\t" (nth 1 eqn) "\n")))
    (setq-local outfilename "variables.txt")
    (setq deqn-write-string deqn-text/equations)
    (setq outbuffername (generate-new-buffer outfilename))
    (with-current-buffer outbuffername
      (insert deqn-write-string)
      (save-buffer))
    (message "wrote equations")
    ;; Write parameters
    (setq-local deqn-text/parameters "# parameters\n")
    (dolist (pars deqn-param-vals)
      (setq-local deqn-text/parameters (concat deqn-text/parameters
                                              (nth 0 pars) "\t" (nth 1 pars) "\n")))
    (setq-local outfilename "parameters.txt")
    (setq outbuffername (generate-new-buffer outfilename))
    (setq deqn-write-string deqn-text/parameters)
    (with-current-buffer outbuffername
      (insert deqn-write-string)
      (save-buffer))
    ;; Write initial conditions
    (setq-local deqn-text/ics "# ics\n")
    (dolist (ics deqn-init-conds)
      (setq-local deqn-text/ics (concat deqn-text/ics
                                              (nth 0 ics) "\t" (nth 1 ics) "\n")))
    (setq-local outfilename "initialconditions.txt")
    (setq outbuffername (generate-new-buffer outfilename))
    (setq deqn-write-string deqn-text/ics)
    (with-current-buffer outbuffername
      (insert deqn-write-string)
      (save-buffer))    
    )

(provide 'deqn-text)
