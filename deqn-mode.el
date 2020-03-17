;;; deqn-mode --- Create and export ODE models to various formats

;;; Commentary:
;; Equation mode is an equation editor that allows
;; the easy creation and simulation of ODEs or Boolean models in
;; Emacs with easy exports to python or matlab.
;; 1. DONE Fontify an equation file
;; 2. DONE Export validated model to PyDSTool
;; 3. DONE Export to plain text (separate files for equations, ics, parameters)
;; 4. DONE Export model to SBML format
;; 5. PROG Export to Python
;;; Code:
(require 'subr-x)
(require 'cl)
(require 'deqn-utils)
(require 'deqn-python)
(require 'deqn-pydstool)
(require 'deqn-text)
(require 'deqn-sbml)


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
  ("P" deqn-pydstool/translate-model "PyDSTool")
  ("p" deqn-python/translate-model "Python")
  ("t" deqn-text/translate-model "Text")
  ("S" deqn-sbml/translate-model "SBML")
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
