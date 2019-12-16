;;; deqn-python.el --- deqn's pure python export

;;; Commentary:

;;; Code:

(require 'cl)
(require 'deqn-utils)

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

(provide 'deqn-python)
