;;; deqn-parameter-sets.el --- Interface to select parameter sets
;;; Commentary:
;;; Provide tabulated list mode view of parameter sets read from CSV file
;;; STATUS: Incomplete
;;; TODO Figure out a good design to pop into the parameter set selection buffer,
;;;      make a selection, and then pop back into the simulator interface
;;;      1. Use some save-excursion type magic to accomplish this
;;;      2. I want to use tabulated list mode rather than completing-read because
;;;         I want to be able to TAB to show the actual parameter values.
;;;      3. Make the tabulated list buffer read only, syntax highlighted.
;;; Code:

(require 'transient)

(defun deqn-read-parameter-table ()
  "Prompt user for pset tabel."
  (interactive)
      (switch-to-buffer "*parameter-sets*")
    (deqn-parameter-sets-mode)
  (let* ((pset-fname (read-file-name "Specify parameter set  CSV file:"))
         (pset-ids (split-string (shell-command-to-string (concat "cut -f1 -d, " pset-fname)) "\n"))
         (pset-raw (shell-command-to-string (concat "cat " pset-fname)))
         (columns [("Parameter Set Name" 50)])
         (rows (cdr (mapcar (lambda (x) `(nil [,x]))
                       (split-string
                        (shell-command-to-string (concat "cut -f1 -d, " pset-fname))
                        "\n"))))
         )
    ;; (message "%s" pset-fname)
    ;;(message "%s" rows)
    ;; (message "%s" pset-raw)
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    ))

(defun select-parameter-set (&optional args)
  "Return parameter set ID.
Supply optional ARGS."
  (interactive (list (transient-args 'parameter-set-options)))
  (let ((pset-selection (aref (tabulated-list-get-entry) 0)))
    (message "You have selected %s" pset-selection))
  )

(define-transient-command parameter-set-options-transient ()
  "Takes switches while simulating ODE model"
  ["Parameter Sets"
   ("s" "Select Parameter set" select-parameter-set)])

(progn
  (setq deqn-parameter-sets-mode-map (make-sparse-keymap))
  (define-key deqn-parameter-sets-mode-map (kbd "C-c h") 'parameter-set-options-transient))

(define-derived-mode deqn-parameter-sets-mode tabulated-list-mode "deqn-par"
  "major mode for displaying parameter sets")

(provide 'deqn-parameter-sets-mode)
;;; deqn-parameter-sets-mode ends here
