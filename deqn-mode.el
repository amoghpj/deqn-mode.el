;;; deqn-mode --- Create and export ODE models to various formats

;;; Commentary:
;; Equation mode is an equation editor that allows
;; the easy creation and simulation of ODEs or Boolean models in
;; Emacs with easy exports to python or matlab.
;; 1. DONE Fontify an equation file
;; 2. DONE Export validated model to PyDSTool
;; 3. DONE Export to plain text (separate files for equations, ics, parameters)
;; 4. DONE Export model to SBML format
;;; Code:
(require 'subr-x)
(require 'cl)
;; (defun deqn/init-buffers ()
;;   "Create window configuration.

;;    Save windows, close windows, split screen vertically so that
;;    the user defined equations on the left are formatted correctly
;;    on the right."
;;   (save-excursion  (generate-new-buffer "*deqn-editor*")
;;                    (generate-new-buffer "deqn-output.org")
;;                    (delete-other-windows)
;;                    (split-window-right)
;;                    (other-window 1)
;;                    (set-buffer "*deqn-editor*")
;;                    (other-window 1)
;;                    (set-buffer "deqn-output.org")))

;; (defgroup deqns nil
;;   "Syntax highlighting for a tab separated variables.txt file."
;;   :prefix "deqns/"
;;   )

(defcustom deqn-separator-symbol "="
  "The equation separator.")

(defcustom deqn-python-folder-path "~/"
  "Please specify the path to custom python scripts.")

(defvar deqn-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments
    ;; Copied from https://repo.or.cz/w/emacs.git/blob/HEAD:/lisp/progmodes/sh-script.el
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table))

(defvar-local deqn-variables ())

(defvar-local deqn-buffer-lines ())

(defvar-local deqn-parameters ())

(defvar-local deqn-equations ())

(defvar-local deqn-init-conds ())

(defvar-local deqn-param-vals ())

(defvar-local valid-ics-p nil)
(defvar-local valid-equations-p nil)
(defvar-local valid-pvals-p nil)

(defconst deqn-operators '("+" "-" "," "*" "^" "/" "(" ")"))

(defcustom deqn-special '("sin" "cos" "shs" "min" "max" "exp")
  "List of reserved function names.")

(defcustom deqn-constants '("pi")
  "List of reserved constant names.")

(defun deqn-get-variables (deqn-buffer-lines)
  "Get variables from buffer.
DEQN-BUFFER-LINES contain useful lines"
  
  (message "Parsing variables")
  (setq-local counter 0)
  (while (< counter (length deqn-buffer-lines))
    (setq-local curr-string (nth counter deqn-buffer-lines))
    (message "processed: %s" curr-string)
    (if (and (string-match-p (regexp-quote "=") curr-string)
             (string= (substring
                       (string-trim
                        (nth 0 (split-string curr-string "=")))
                       0 1) "d")) ;; << If there is an equal sign and LHS starts with d
        (add-to-list 'deqn-variables
                     (substring (string-trim
                                 (nth 0
                                      (split-string curr-string "=")))
                                1)))
    (setq-local counter (+ 1 counter))))

(defun deqn-create-maps (deqn-buffer-lines)
  "Make a list of lists associating variables to their respective equations.
DEQN-BUFFER-LINES stores the useful lines."
  (dolist (bline deqn-buffer-lines)
    (setq-local curr-item-split
                (split-string bline  "="))
    (setq-local curr-split-trunc ())
    (add-to-list 'curr-split-trunc (string-trim (nth 0 curr-item-split)))
    (add-to-list 'curr-split-trunc (string-trim (nth 1 curr-item-split)))
    (setq-local curr-split-trunc (reverse curr-split-trunc))
    (message "%s" curr-split-trunc)
    (cond
     ((member (string-trim (nth 0 curr-split-trunc)) deqn-variables)
      (add-to-list 'deqn-init-conds curr-split-trunc))
     ((member (substring (string-trim (nth 0 curr-split-trunc)) 1) deqn-variables)
      (add-to-list  'deqn-equations curr-split-trunc))
     ((member (string-trim (nth 0 curr-split-trunc)) deqn-parameters)
      (add-to-list 'deqn-param-vals curr-split-trunc)))
    ))

(defun deqn-get-useful-lines ()
  "Strip empty lines and comments from buffer."
  (message "Remove noise")
  (goto-char (point-min)) ;; << go to top of buffer
  (while (not (eobp)) ;; <<  start while loop as long as not end of buffer
    (setq-local curr-string
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))) ;; << read the first line
    (if (or (eq (length curr-string) 0)
            (string= (substring curr-string 0 1) "#")) ;; << if comment or empty go forward
        (forward-line 1)
      (if (string-match-p (regexp-quote "#") curr-string) ;; << comment in line
          (setq-local curr-string (nth 0 (split-string curr-string "#")))) ;; << remove comment
      (message "%s" curr-string)
      (add-to-list 'deqn-buffer-lines (string-trim curr-string))
      (forward-line 1))))
  
(defun deqn-get-parameters (deqn-buffer-lines)
  "Get variables from buffer.
DEQN-BUFFER-LINES contains the useful part of the buffer"
  (message "Parsing parameters")
  (setq-local counter 0)
  (message "%s"  deqn-buffer-lines)
  (dolist (bline deqn-buffer-lines)
    (when (string= (substring (string-trim (nth 0 (split-string bline "="))) 0 1) "d")
      (setq-local curr-rhs (nth 1 (split-string bline "=")));; << Look at the RHS
      ;; Go through every operator and replace with space
      (setq-local curr-rhs (remove-kwords curr-rhs deqn-operators))
      (message "processed: %s" curr-rhs)
      ;; Split string
      (setq curr-rhs-list (split-string curr-rhs))
      (message "RHS list has %s items" (length curr-rhs-list))
      ;; Remove duplicates
      (setq-local curr-rhs-list (delete-dups curr-rhs-list))
      (message "\tdropping dupes:  %s" (length curr-rhs-list))
      ;; Go through every constant and replace with space
      (setq-local curr-rhs-list (custom-set-difference curr-rhs-list deqn-constants))
      (message "\tdropping constants: %s" (length curr-rhs-list))
      ;; ;; Go through every special function and replace with space
      (setq-local curr-rhs-list (custom-set-difference curr-rhs-list deqn-special))
      (message "\tdropping special: %s" (length curr-rhs-list))
      ;; Go through all the variables and replace with space
      (setq-local curr-rhs-list (custom-set-difference curr-rhs-list deqn-variables))
      (message "\tdropping vars: %s" (length curr-rhs-list))
      (dolist (rhsitem curr-rhs-list)
        (if (not (ensure-number rhsitem))
            (add-to-list 'deqn-parameters (string-trim rhsitem))
          nil))
    deqn-parameters
    )))

(defun ensure-number (arg-string)
  "Checks if ARG-STRING is composed purely of numbers."
(setq-local deqn-constitutes-number '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "."))
(setq-local deqn-number-p t)
(setq-local string-constituents (delete "" (split-string arg-string "")))
(dolist (stcon string-constituents)
  (if (not (member stcon deqn-constitutes-number))
      (setq-local deqn-number-p nil)
    nil))
deqn-number-p)

(defun remove-kwords (deq-string kwlist)
  "From DEQ-STRING, remove each item in KWLIST."
  (setq-local counter 0)
  ;;  (message "Length of KWIST is %s" (length kwlist))
  (while (< counter (length kwlist))
    ;;    (message "AT iter [%s] DEQ-STRING IS: %s" counter deq-string)
    (setq-local curr-item (nth counter kwlist))
    ;;    (message "STARTING AT %s" curr-item)
    (if (string-match-p curr-item deq-string)
        (setq-local deq-string (replace-regexp-in-string (regexp-quote curr-item) " " deq-string)))
    (setq-local counter (+ counter 1)))
  deq-string)

(defun custom-set-difference (listA listB)
  "Find difference of lists LISTA and LISTB."
  (setq-local counter 0)
  (setq-local unique-in-A ())
  (while (< counter (length listA))
    (if (not (member (nth counter listA) listB))
        (add-to-list 'unique-in-A (nth counter listA)))
    (setq-local counter (+ 1 counter)))
  unique-in-A
  )

(defun deqn-parse-and-validate-buffer ()
  "Parse buffer for variables and parameters."
  (deqn-get-useful-lines)
  (message "%s" deqn-buffer-lines)
  (deqn-get-variables deqn-buffer-lines)
  (message "%s" deqn-variables)
  (deqn-get-parameters deqn-buffer-lines)
  (message "%s" deqn-parameters)
  (deqn-create-maps deqn-buffer-lines)
  ;; Next carry out some checks
  (if (= (length deqn-variables) (length deqn-equations))
      (setq-local valid-equations-p t)
    (message "%s" (length deqn-equations))
    (error "ERROR IN EQUATIONS.  Remember, parameters can't start with a d!"))
  (if (= (length deqn-variables) (length deqn-init-conds))
      (setq-local valid-ics-p t)
    (error "ERROR IN ICS.  Please specify initial conditions of all variables!"))
  (if (= (length deqn-parameters) (length deqn-param-vals))
      (setq-local valid-pval-p t)
    ;; Check if every parameter has a value, error out if not
    (setq-local deqn-pars-with-vals ())
    (dolist (dpar deqn-param-vals)
      (add-to-list 'deqn-pars-with-vals (nth 0 dpar)))
    (dolist (par deqn-parameters)
      (if (not (member par deqn-pars-with-vals))
          (error "ERROR IN PARAMETER VALUES.  %s does not have a value!" par))))
  (if (and valid-equations-p valid-ics-p valid-pval-p)
      (message "This file has been successfully validated")
    (message "something went wrong :("))
  )

(defun deqn-write-pydstool ()
  "Export current file to PyDSTool format in python."
  (interactive)
  (deqn-parse-and-validate-buffer)
  (setq-local deqn-pydstool/varspecs "{\n")
  (dolist (eqn deqn-equations)
    (setq-local deqn-pydstool/varspecs
                (concat deqn-pydstool/varspecs "		      '"
                        (substring (nth 0 eqn) 1) "' : '" (nth 1 eqn) "',\n" )))
  (setq-local deqn-pydstool/varspecs (concat deqn-pydstool/varspecs "	          }"))
  (setq-local deqn-pydstool/pars "{\n")
  (dolist (pvals deqn-param-vals)
    (setq-local deqn-pydstool/pars
                (concat deqn-pydstool/pars
                        "		      '"
                        (nth 0 pvals) "' : " (nth 1 pvals)
                        ",\n" )))
  (setq-local deqn-pydstool/pars (concat deqn-pydstool/pars
                                         "	          }"))
  (setq-local deqn-pydstool/ics "{\n")
  (dolist (ics deqn-init-conds)
    (setq-local deqn-pydstool/ics
                (concat deqn-pydstool/ics
                        "		      '"
                        (nth 0 ics) "' : " (nth 1 ics)
                        ",\n" )))
  (setq-local deqn-pydstool/ics (concat deqn-pydstool/ics
                                        "	          }"))
  (setq deqn-write-string
              (concat "## This model file was generated automatically using deqn-mode.el ##\n"
                      "import PyDSTool as dst\n"
                      "import matplotlib.pyplot as plt\n\n"
                      "DSargs = dst.args(name='test',\n"
                      "                  varspecs="
                      deqn-pydstool/varspecs
                      ",\n                  pars="
                      deqn-pydstool/pars
                      ",\n                  ics="
                      deqn-pydstool/ics
                      ",\n                  fnspecs={'shs':(['sig','summation'],'1/(1+e^(-sig*summation))')}"
                      ",\n                  tdata=[0,10])"
                      "\n\nDS = dst.Vode_ODEsystem(DSargs)\n"
                      "p = DS.compute('test').sample()"))
  ;; Write to file
  (setq-local outfilename (concat (file-name-base (buffer-file-name)) ".py"))
  (setq outbuffername (generate-new-buffer outfilename))
  (with-current-buffer outbuffername
    (insert deqn-write-string)
    (save-buffer))
  )

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

(defun deqn-write-python ()
    "Export current file to a python function file"
    (interactive)
    (deqn-parse-and-validate-buffer)
    ;; Write equations
    (message "Starting file construction")
    (setq-local deqn-python/body (concat
                                  "# This file was generated automatically using deqn.el\n"
                                  "from scipy.integrate import odeint\n"
                                  "import numpy as np\n"
                                  "import matplotlib.pyplot as plt\n"
                                  "def mymodel(X, t, pars):\n"))
    ;; Write parameters
    (setq-local counter 0)    
    (dolist (pars deqn-param-vals)
      (setq-local deqn-python/body (concat deqn-python/body
                                           "    " (nth 0 pars) "= pars[" (number-to-string counter) "]\n"))
      (setq-local counter(+ 1 counter)))
    ;; Initialize interpretable variables with argument X
    (setq-local counter 0)
    (dolist (eqn deqn-equations)
      (setq-local deqn-python/body  (concat deqn-python/body
                                            "    " (substring (nth 0 eqn) 1) "="
                                            (concat "X[" (number-to-string counter) "]") "\n\n"))
                  (setq-local counter (+ 1 counter)))
    ;; Specify equations
    (dolist (eqn deqn-equations)
      (setq-local deqn-python/body  (concat deqn-python/body
              "    " (nth 0 eqn) "=" (replace-regexp-in-string (regexp-quote "exp") "np.exp" (nth 1 eqn)) "\n")))
    ;; Make return value list dX containing derivatives
    (setq-local deqn-python/body (concat deqn-python/body "    dX = ["))
    (dolist (eqn deqn-equations)
      (setq-local deqn-python/body (concat deqn-python/body
               (nth 0 eqn) ", ")) )
    (setq-local deqn-python/body (concat deqn-python/body "]\n"))
    (setq-local deqn-python/body (concat deqn-python/body "    return dX\n"))
    ;; End of function definition
    ;; Create variable to hold initial conditions
    (setq-local deqn-python/body (concat deqn-python/body "x0 = ["))
    (dolist (ics deqn-init-conds)
      (setq-local deqn-python/body (concat deqn-python/body
                                           (nth 1 ics) ",# " (nth 0 ics) "\n")))
    (setq-local deqn-python/body (concat deqn-python/body "]\n"
                                         "pars=["))    
    (dolist (pars deqn-param-vals)
      (setq-local deqn-python/body (concat deqn-python/body
                                            (nth 1 pars) ", # " (nth 0 pars) "\n")))

    (setq-local deqn-python/body (concat deqn-python/body "]\n"
                                         "t = np.linspace(0,10, 100)\n"
                                         "Y = odeint(mymodel, x0, t, (pars,))\n"
                                         "for i in range(len(Y[0])):\n"
                                         "    plt.plot(t, Y[:,i])\n"
                                         "plt.show()\n"
                                         ))    
    (setq-local outfilename "*deqn-python-model*")
    (setq deqn-write-string deqn-python/body)
    (setq outbuffername (generate-new-buffer outfilename))
    (with-current-buffer outbuffername
      (insert deqn-write-string)
      (save-buffer))    
    )

(defun deqn-write-sbml ()
    "Export current file to the SBML format.
Calls the smbl-translator.py script to read plain text files 
containing the model definition, and exports to SBML."
    (interactive)
    (deqn-write-text)
    (shell-command  (concat "python " deqn-python-folder-path "sbml-translator.py")))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.eqn\\'" . deqn-mode))

(defvar deqn-highlights
  '(("pi" . font-lock-constant-face)
 ( " sin\\| exp\\| cos\\| shs\\| min\\| max\\|=" . font-lock-function-name-face) ;;     (deqn-special-names  . font-lock-function-name-face)
    ( "\\([a-zA-Z0-9_]*\\)[ ]*=" 1 font-lock-keyword-face))) ;;         ( (concat "\\([a-zA-Z0-9_]*\\)[ ]*" deqn-separator-symbol)  1 font-lock-keyword-face)))


(defhydra deqn-mode-hydra (:color blue)
  "
           ----------------
           ^Export Options^
           ----------------
              _P_yDSTool    
              _p_ython
              _S_BML
              _t_ext
"
  ("P" deqn-write-pydstool "PyDSTool")
  ("p" deqn-write-python "Python")  
  ("t" deqn-write-text "Text")
  ("S" deqn-write-sbml "SBML")  
  )

(progn
  (setq deqn-mode-map (make-sparse-keymap))

  (define-key deqn-mode-map (kbd "C-c h") 'deqn-mode-hydra/body)
  )

(define-derived-mode deqn-mode prog-mode "deqn"
  "major mode for editing equations."
  ;; HOLD (deqn/init-buffers)
  (setq font-lock-defaults '(deqn-highlights))
  )

(provide 'deqn-mode)
;;; deqn-mode.el ends here
