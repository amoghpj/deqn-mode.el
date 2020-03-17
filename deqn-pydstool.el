;;; deqn-pydstool.el --- deqn's PyDSTool export

;;; Commentary:

;;; Code:

(require 'cl)
(require 'deqn-utils)

(defun deqn-pydstool/translate-model ()
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

(provide 'deqn-pydstool)
