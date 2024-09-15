;;; genexpr-mode.el --- a major-mode for editing GenExpr files  -*- lexical-binding: t -*-

;; modified from:
;; simpc-mode: https://github.com/rexim/simpc-mode
;; GenExprForSublime: https://github.com/emmanueljourdan/GenExprForSublime
;; lua-mode: https://github.com/immerrr/lua-mode

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

(defun genexpr-font-lock-keywords ()
  (list
   `(,(regexp-opt genexpr--builtins 'symbols) . font-lock-builtin-face)
   `(,(regexp-opt genexpr--constants 'symbols) . font-lock-constant-face)
   `(,(regexp-opt genexpr--keywords 'symbols) . font-lock-keyword-face)))

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
