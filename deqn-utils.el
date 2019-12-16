;;; deqn-utils.el --- Utilities for deqn mode

;;; Commentary:

;;; Code:

(require 'subr-x)
(require 'cl)

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

(provide 'deqn-utils)
