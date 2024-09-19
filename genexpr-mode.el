;;; genexpr-mode.el --- Major mode for editing GenExpr files -*- lexical-binding: t; -*-

;; Author: Zhao Shenyang <dev@zsy.im>
;;
;; URL: https://github.com/larme/genexpr-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Keywords: languages, dsp

;;; Commentary:

;; This package provides a major mode for editing GenExpr files used in
;; Max/MSP's gen~ object.  It includes syntax highlighting and indentation
;; support for GenExpr code.

;; To use this mode, simply open a .genexpr file or add the following to
;; your Emacs configuration:

;; (require 'genexpr-mode)
;; (add-to-list 'auto-mode-alist '("\\.genexpr\\'" . genexpr-mode))

;;; Code:

(require 'subr-x)

(defgroup genexpr nil
  "Major mode for editing GenExpr code."
  :prefix "genexpr-"
  :group 'languages)

(defcustom genexpr-indent-level 4
  "Amount by which GenExpr subexpressions are indented."
  :type 'integer
  :group 'genexpr
  :safe #'integerp)

(defvar genexpr--electric-indent-chars
  (mapcar #'string-to-char '("}" "]" ")" ";")))

(defvar genexpr-mode-map
  (let ((result-map (make-sparse-keymap)))
    (unless (boundp 'electric-indent-chars)
      (mapc (lambda (electric-char)
              (define-key result-map
                (read-kbd-macro
                 (char-to-string electric-char))
                #'genexpr-electric-match))
            genexpr--electric-indent-chars))
    result-map)
  "Keymap used in genexpr-mode buffers.")

(defun genexpr-electric-match (arg)
  "Insert character and adjust indentation."
  (interactive "P")
  (let (blink-paren-function)
    (self-insert-command (prefix-numeric-value arg)))
  (if genexpr-electric-flag
      (genexpr-indent-line))
  (blink-matching-open))


(defvar genexpr-electric-flag t
  "If t, electric actions (like automatic reindentation) will happen when an electric
 key like `{' is pressed")
(make-variable-buffer-local 'genexpr-electric-flag)

(defvar genexpr-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C/C++ style comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(defconst genexpr--constants
  '("constant" "degtorad" "DEGTORAD" "e" "E" "f" "fftfullspect" "FFTFULLSPECT" "ffthop" "FFTHOP" "fftoffset" "FFTOFFSET" "fftsize" "FFTSIZE"
    "float" "halfpi" "HALFPI" "i" "int" "invpi" "INVPI" "ln10" "LN10" "ln2" "LN2" "log10e" "LOG10E" "log2e" "LOG2E" "pi" "PI"
    "radtodeg" "RADTODEG" "samplerate" "SAMPLERATE" "sqrt1_2" "SQRT1_2" "sqrt2" "SQRT2" "twopi" "TWOPI" "vectorsize" "VECTORSIZE"))

(defconst genexpr--builtins
  '(
    ;; gen common operators, ref: https://docs.cycling74.com/max8/vignettes/gen_common_operators
    "neqp" "gt" "eq" "eqp" "gte" "gtp" "lt" "lte" "ltep" "ltp" "max" "maximum" "min" "minimum" "neq" "step"
    "Param"
    "expr"
    "pass"
    "rmod" "rsub" "mod" "add" "sub" "div" "absdiff" "cartopol" "mul" "neg" "poltocar" "rdiv"
    "abs" "ceil" "floor" "trunc" "fract" "sign"
    "exp" "exp2" "fastexp" "fastpow" "ln" "log" "log10" "log2" "pow" "sqrt"
    "clamp" "clip" "fold" "scale" "wrap"
    "switch" "gate" "mix" "selector" "smoothstep"
    "acos" "acosh" "asin" "asinh" "atan" "atan2" "atanh" "cos" "cosh" "degrees" "fastcos" "fastsin" "fasttan" "hypot" "radians" "sin" "sinh" "tan" "tanh"
    "noise"

    ;; gen~ operators ref: https://docs.cycling74.com/max8/vignettes/gen~_operators
    "Buffer" "channels" "cycle" "Data" "dim" "lookup" "nearest" "peek" "poke" "sample" "splat" "wave"
    "atodb" "dbtoa" "ftom" "mstosamps" "mtof" "sampstoms"
    "fixdenorm" "fixnan" "isdenorm" "isnan"
    "Delay" "History"
    "fftinfo"
    "change" "dcblock" "delta" "interp" "latch" "phasewrap" "sah" "slide"
    "mulequals" "accum" "plusequals" "counter"
    "round"
    "phasor" "rate" "train" "triangle"))

(defconst genexpr--keywords
  '("break" "continue" "else" "for" "if" "return" "while"
    "in" "out"
    "in1" "in2" "in3" "in4" "in5" "in6" "in7" "in8" "in9"
    "out1" "out2" "out3" "out4" "out5" "out6" "out7" "out8" "out9"
    "in10" "in11" "in12" "in13" "in14" "in15" "in16" "in17" "in18" "in19"
    "out10" "out11" "out12" "out13" "out14" "out15" "out16" "out17" "out18" "out19"))

(eval-when-compile
  ;; Silence compilation warning about `compilation-error-regexp-alist' defined
  ;; in compile.el.
  (require 'compile))

(eval-and-compile
  (progn
    ;; Emacs 27+ way of customizing rx
    (defvar genexpr--rx-bindings)
    (setq
     genexpr--rx-bindings
     `((symbol (&rest x) (seq symbol-start (or x) symbol-end))
       (ws (* (any " \t")))
       (ws+ (+ (any " \t")))
       (wsn (* (any " \t\n")))

       (genexpr-name (symbol (seq (+ (any alpha "_")) (* (any alnum "_")))))
       (genexpr-number
	(seq (or (seq (+ digit) (opt ".") (* digit))
                 (seq (* digit) (opt ".") (+ digit)))
             (opt (regexp "[eE][+-]?[0-9]+"))))
       (genexpr-funcheader-with-up-to-8-variables
	(seq bol (group-n 1 genexpr-name) ws "("
	     (group-n 2 genexpr-name) ws
             (? "," ws (group-n 3 genexpr-name) ws
		(? "," ws (group-n 4 genexpr-name) ws
		   (? "," ws (group-n 5 genexpr-name) ws
                      (? "," ws (group-n 6 genexpr-name) ws
			 (? "," ws (group-n 7 genexpr-name) ws
                            (? "," ws (group-n 8 genexpr-name) ws
                               (? "," ws (group-n 9 genexpr-name) ws)))))))
	     ")" wsn "{"))))

    (defmacro genexpr-rx (&rest regexps)
      (eval `(rx-let ,genexpr--rx-bindings
               (rx ,@regexps))))

    (defun genexpr-rx-to-string (form &optional no-group)
      (rx-let-eval genexpr--rx-bindings
        (rx-to-string form no-group)))))

(defun genexpr-font-lock-keywords ()
  (list
   `(,(regexp-opt genexpr--builtins 'symbols) . font-lock-builtin-face)
   `(,(regexp-opt genexpr--constants 'symbols) . font-lock-constant-face)
   `(,(regexp-opt genexpr--keywords 'symbols) . font-lock-keyword-face)
   `(,(genexpr-rx genexpr-funcheader-with-up-to-8-variables)
     (1 font-lock-function-name-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-variable-name-face nil noerror)
     (4 font-lock-variable-name-face nil noerror)
     (5 font-lock-variable-name-face nil noerror)
     (6 font-lock-variable-name-face nil noerror)
     (7 font-lock-variable-name-face nil noerror)
     (8 font-lock-variable-name-face nil noerror)
     (9 font-lock-variable-name-face nil noerror))))

(defun genexpr--previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun genexpr--indentation-of-previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (current-indentation)))

(defun genexpr--desired-indentation ()
  (let* ((cur-line (string-trim-right (thing-at-point 'line t)))
         (prev-line (string-trim-right (genexpr--previous-non-empty-line)))
         (indent-len genexpr-indent-level)
         (prev-indent (genexpr--indentation-of-previous-non-empty-line)))
    (cond
     ((string-match-p "^\\s-*switch\\s-*(.+)" prev-line)
      prev-indent)
     ((and (string-suffix-p "{" prev-line)
           (string-prefix-p "}" (string-trim-left cur-line)))
      prev-indent)
     ((string-suffix-p "{" prev-line)
      (+ prev-indent indent-len))
     ((string-prefix-p "}" (string-trim-left cur-line))
      (max (- prev-indent indent-len) 0))
     ((string-suffix-p ":" prev-line)
      (if (string-suffix-p ":" cur-line)
          prev-indent
        (+ prev-indent indent-len)))
     ((string-suffix-p ":" cur-line)
      (max (- prev-indent indent-len) 0))
     (t prev-indent))))

;;; TODO: customizable indentation (amount of spaces, tabs, etc)
(defun genexpr-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation
            (genexpr--desired-indentation))
           (n (max (- (current-column) (current-indentation)) 0)))
      (indent-line-to desired-indentation)
      (forward-char n))))

;;;###autoload
(define-derived-mode genexpr-mode prog-mode "GenExpr"
  "Simple major mode for editing GenExpr files."
  :syntax-table genexpr-mode-syntax-table
  :group 'genexpr
  (setq-local font-lock-defaults '(genexpr-font-lock-keywords))
  (setq-local indent-line-function 'genexpr-indent-line)
  (setq-local comment-start "// ")
  (when (boundp 'electric-indent-chars)
    ;; If electric-indent-chars is not defined, electric indentation is done
    ;; via `genexpr-mode-map'.
    (setq-local electric-indent-chars
                (append electric-indent-chars genexpr--electric-indent-chars))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.genexpr\\'" . genexpr-mode))

(provide 'genexpr-mode)

;;; genexpr-mode.el ends here
