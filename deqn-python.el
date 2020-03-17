;;; deqn-python.el --- deqn's pure python export

;;; Commentary:

;;; Code:

(require 'cl)
(require 'deqn-utils)

(defcustom deqn-python/header-string (concat
                                  "# This file was generated automatically using deqn.el\n"
                                  "from scipy.integrate import odeint\n"
                                  "import numpy as np\n"
                                  "import matplotlib.pyplot as plt\n\n"
                                  "class Model:\n"
                                  "    def model(self, X, t, pars):\n")
  "Default header for pure python export.
Can be customized to change the library imports.")

(defun deqn-python/translate-model ()
    "Export current file to a python function file."
    (interactive)
    (deqn-parse-and-validate-buffer)
    
    (setq-local deqn-python/body deqn-python/header-string)
    ;; Write parameters
    (setq-local counter 0)    
    (dolist (pars deqn-param-vals)
      (setq-local deqn-python/body (concat deqn-python/body
                                           "        " (nth 0 pars) "= pars[" (number-to-string counter) "]\n"))
      (setq-local counter(+ 1 counter)))
    ;; Initialize interpretable variables with argument X
    (setq-local counter 0)
    (dolist (eqn deqn-equations)
      (setq-local deqn-python/body  (concat deqn-python/body
                                            "        " (substring (nth 0 eqn) 1) "="
                                            (concat "X[" (number-to-string counter) "]") "\n"))
      (setq-local counter (+ 1 counter)))
    
    ;; Specify equations
    (dolist (eqn deqn-equations)
      (setq-local deqn-python/body  (concat deqn-python/body
              "        " (nth 0 eqn) "=" (replace-regexp-in-string (regexp-quote "exp") "np.exp" (nth 1 eqn)) "\n")))
    ;; Make return value list dX containing derivatives
    (setq-local deqn-python/body (concat deqn-python/body "        dX = ["))
    (dolist (eqn deqn-equations)
      (setq-local deqn-python/body (concat deqn-python/body
               (nth 0 eqn) ", ")) )
    (setq-local deqn-python/body (concat deqn-python/body "]\n"))
    (setq-local deqn-python/body (concat deqn-python/body "        return dX\n\n"))

    ;; Create dictionaries to hold ics and mapper
    (setq-local deqn-python/body (concat deqn-python/body 
                                         "    def __init__(self):\n"
                                         "        self.tmax = 10\n"
                                         "        self.nsteps = 100\n"))
    (setq-local deqn-python/icsdict "        self.icsdict = {\n")
    (setq-local deqn-python/varmapper "        self.varmapper = {\n")    
    (setq-local counter 0)        
    (dolist (ics deqn-init-conds)
      (setq-local deqn-python/icsdict (concat deqn-python/icsdict
                                              "            '" (nth 0 ics) "':" (nth 1 ics) ",\n"))
      (setq-local deqn-python/varmapper (concat deqn-python/varmapper
                                                "            " (number-to-string counter) ":'" (nth 0 ics) "',\n"))
      (setq-local counter (+ 1 counter)))
    (setq-local deqn-python/varmapper (concat deqn-python/varmapper "        }\n"))
    (setq-local deqn-python/icsdict (concat deqn-python/icsdict "        }\n"))    

    ;; Create dictionaries to hold pars and mapper 
    (setq-local deqn-python/pardict "        self.pardict = {\n")        
    (setq-local deqn-python/parmapper "        self.parmapper = {\n")    
    (setq-local counter 0)        
    (dolist (ics deqn-param-vals)
      (setq-local deqn-python/pardict (concat deqn-python/pardict
                                              "            '" (nth 0 ics) "':" (nth 1 ics) ",\n"))
      (setq-local deqn-python/parmapper (concat deqn-python/parmapper
                                                "            " (number-to-string counter) ":'" (nth 0 ics) "',\n"))
      (setq-local counter (+ 1 counter)))
    
    (setq-local deqn-python/pardict (concat deqn-python/pardict "        }\n"))
    (setq-local deqn-python/parmapper (concat deqn-python/parmapper "        }\n"))    
    
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
                                         "    def simulate(self):\n"
                                         "        x0 = []\n"
                                         "        for i in range(len(self.icsdict.keys())):\n"
                                         "            x0.append(self.icsdict[self.varmapper[i]])\n\n"  
                                         "        pars = []\n" 
                                         "        for i in range(len(self.pardict.keys())):\n"
                                         "            pars.append(self.pardict[self.parmapper[i]])\n"
                                         "        t = np.linspace(0,self.tmax, self.nsteps)\n"
                                         "        Y = odeint(self.model, x0, t, (pars,))\n"
                                         "        ydict = {self.varmapper[vind]:Y[:,vind] for vind in range(len(self.icsdict))}\n\n"
                                         "        return(t, ydict)\n\n"
                                         "    def set_tmax(self, tmax):\n"
                                         "        self.tmax = tmax\n\n"
                                         "    def set_nsteps(self, nsteps):\n"
                                         "        self.nsteps = nsteps\n\n"                                         
                                         "    def set_parameter(self, pardict):\n"
                                         "        for k,v in pardict.items():\n"
                                         "            if k in self.pardict.keys():\n"
                                         "                self.pardict[k] = v\n\n"
                                         "    def set_ics(self, icsdict):\n"
                                         "        for k,v in icsdict.items():\n"
                                         "            if k in self.icsdict.keys():\n"
                                         "                self.icsdict[k] = v\n\n"                                         
                                         "    def plotall(self, t, Y):\n"
                                         "        for i in range(len(Y[0])):\n"
                                         "            plt.plot(t, Y[:,i])\n"
                                         "        plt.show()\n"
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
