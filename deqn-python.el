;;; deqn-python.el --- deqn's pure python export

;;; Commentary:

;;; Code:

(require 'cl)
(require 'deqn-utils)

(defcustom deqn-python/header-string (concat
                                  "# This file was generated automatically using deqn.el\n"
                                  "from scipy.integrate import odeint\n"
                                  "import numpy as np\n"
                                  "import matplotlib.pyplot as plt\n"
                                  "def mymodel(X, t, pars):\n")
  "Default header for pure python export.
Can be customized to change the library imports.")

(defun deqn-write-python ()
    "Export current file to a python function file."
    (interactive)
    (deqn-parse-and-validate-buffer)
    
    (setq-local deqn-python/body deqn-python/header-string)
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
                                            (concat "X[" (number-to-string counter) "]") "\n"))
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

    ;; Create dictionaries to hold ics and mapper
    (setq-local deqn-python/icsdict "icsdict = {\n")
    (setq-local deqn-python/varmapper "varmapper = {\n")    
    (setq-local counter 0)        
    (dolist (ics deqn-init-conds)
      (setq-local deqn-python/icsdict (concat deqn-python/icsdict
                                              "    '" (nth 0 ics) "':" (nth 1 ics) ",\n"))
      (setq-local deqn-python/varmapper (concat deqn-python/varmapper
                                                "    " (number-to-string counter) ":'" (nth 0 ics) "',\n"))
      (setq-local counter (+ 1 counter)))
    (setq-local deqn-python/varmapper (concat deqn-python/varmapper "}\n"))
    (setq-local deqn-python/icsdict (concat deqn-python/icsdict "}\n"))    

    ;; Create dictionaries to hold pars and mapper 
    (setq-local deqn-python/pardict "pardict = {\n")        
    (setq-local deqn-python/parmapper "parmapper = {\n")    
    (setq-local counter 0)        
    (dolist (ics deqn-param-vals)
      (setq-local deqn-python/pardict (concat deqn-python/pardict
                                              "    '" (nth 0 ics) "':" (nth 1 ics) ",\n"))
      (setq-local deqn-python/parmapper (concat deqn-python/parmapper
                                                "    " (number-to-string counter) ":'" (nth 0 ics) "',\n"))
      (setq-local counter (+ 1 counter)))
    
    (setq-local deqn-python/pardict (concat deqn-python/pardict "}\n"))
    (setq-local deqn-python/parmapper (concat deqn-python/parmapper "}\n"))    
    
    ;; Add mappers and dicts to body
    (setq-local deqn-python/body (concat deqn-python/body
                                         deqn-python/icsdict
                                         deqn-python/varmapper
                                         deqn-python/pardict
                                         deqn-python/parmapper
                                         ))

    ;; Loop over mapper items to create lists of ics and pars that can be passed
    ;; to model function
    (setq-local deqn-python/body (concat deqn-python/body 
                                         "x0 = []\n"
                                         "for i in range(len(icsdict.keys())):\n"
                                         "    x0.append(icsdict[varmapper[i]])\n\n"  
                                         "pars = []\n"
                                         "for i in range(len(pardict.keys())):\n"
                                         "    pars.append(pardict[parmapper[i]])\n"
                                         ))    
    ;; (dolist (pars deqn-param-vals)
    ;;   (setq-local deqn-python/body (concat deqn-python/body
    ;;                                         (nth 1 pars) ", # " (nth 0 pars) "\n")))

    ;; Write function call
    (setq-local deqn-python/body (concat deqn-python/body 
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
;;; deqn-python ends here
