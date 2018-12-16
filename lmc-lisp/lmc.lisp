/*TODO: LBL CON NOME DI ISTRUZIONI*/

(defun read-lines (stream)
    (let ((line (read stream)))
        (when line (cons line (read-lines stream)))))

(defun getCleanLine (line)
    (string-trim " " (subseq line 0 (search "//" line))))

(defun getOp (inst)
    (cond ((string= inst "ADD") "1")
          ((string= inst "SUB") "2")
          ((string= inst "STA") "3")
          ((string= inst "LDA") "5")
          ((string= inst "BRA") "6")
          ((string= inst "BRZ") "7")
          ((string= inst "BRP") "8")
          ((string= inst "INP") "901")
          ((string= inst "OUT") "902")
          ((string= inst "HLT") "0")))

(defun isLabel (arg)
    (and (eq (parse-integer (subseq arg 0 1) :junk-allowed T) nil) (not (getOp arg))))

(defun isValue (arg)
    (eq (length arg) (length (write-to-string (parse-integer arg :junk-allowed T)))))

(defun isValidArg (arg)
    (or (isLabel arg) (isValue arg)))

(defun getPos (insts insts2 lbl)
    (if (string= (first (split-sequence " " (first insts))) lbl) 
        (position (first insts) insts2 :test 'equal)
        (if (eq insts nil) nil (getPos (rest insts) insts2 lbl))))

(defun getValue (insts insts2 arg)
    (if (isValidArg arg)
        (if (isLabel arg) (getPos insts insts2 arg) (parse-integer arg :junk-allowed T)) nil))

(defun getList (insts insts2 inst)
    (format t "getList for ~s" (second (split-sequence " " (first insts))))
    (cons (concatenate 'string (getOp inst) (getValue insts2 insts2 (second (split-sequence " " (first insts))))) nil))

(defun translate (insts insts2)
    (let ((inst (first (split-sequence " " (first insts)))))
        (write inst)
         (if (getOp inst) 
            ((write inst) (cons (concatenate 'string (getOp inst) (getValue insts2 insts2 (second (split-sequence " " (first insts))))) (translate (rest insts) insts2)))
                (if (isLabel inst)
                  (write inst)
                  nil))))

()

 (cons (concatenate 'string (getOp (second (split-sequence " " (first insts)))) (getValue insts2 insts2 (third (split-sequence " " (first insts))))) (translate (rest insts) insts2))

(defparameter *test* (list "ADD 5" "SUB 2"))

(let ((a 'inside) (b a))
    (format nil "~S ~S ~S" a b (dummy-function)))