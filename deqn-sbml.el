;;; deqn-sbml.el --- deqn's SBML export

;;; Commentary:

;;; Code:

(require 'cl)
(require 'deqn-utils)

(defun deqn-write-sbml ()
    "Export current file to the SBML format.
Calls the smbl-translator.py script to read plain text files 
containing the model definition, and exports to SBML."
    (interactive)
    (deqn-write-text)
    (shell-command  (concat "python " deqn-python-folder-path "sbml-translator.py")))

(provide 'deqn-sbml)
