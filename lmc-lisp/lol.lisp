;TODO: LBL CON NOME DI ISTRUZIONI

(defun read-lines (stream)
    (let ((line (read-line stream nil nil)))
        (when line (cons (string-upcase line) (read-lines stream)))))

(defun getCleanLine (line)
    (string-trim " " (subseq line 0 (search "//" line))))

(defun getCleanLines (lines)
    (if lines
        (cons (getCleanLine (first lines)) (getCleanLines (rest lines))) nil))

(defun getOp (inst)
    (cond ((string= inst "ADD") "1")
          ((string= inst "SUB") "2")
          ((string= inst "STA") "3")
          ((string= inst "LDA") "5")
          ((string= inst "BRA") "6")
          ((string= inst "BRZ") "7")
          ((string= inst "BRP") "8")
          ((string= inst "DAT") "0")
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
    (and T (if (string= (first (remove "" (split-sequence " " (first insts))
                                :test #'equal)) lbl)
        (position (first insts) insts2 :test #'iseq)
        (if (eq insts nil) nil (getPos (rest insts) insts2 lbl)))))

(defun iseq (a b)
    (if (string= (first (remove "" (split-sequence " " a) :test #'equal))
        (first (remove "" (split-sequence " " b) :test #'equal)))
        T nil))

(defun getValue (insts insts2 arg)
    (if (and arg (not (eq (length arg) 0)) (isValidArg arg))
        (if (isLabel arg) 
            (let ((pos (getPos insts insts2 arg)))
                (if pos (write-to-string pos) nil))
             arg) nil))

(defun DBLines (file) (getCleanLines (with-open-file (stream file :direction :input) (read-lines stream))))

(defun translate (insts insts2)
    (if insts
        (let ((inst (first (remove "" (split-sequence " " (first insts))
                                     :test #'equal))))
             (if (getOp inst) 
                (cons (write-to-string (parse-integer (concatenate 
                                                        'string (getOp inst)
                                           (if (second 
                                                     (remove "" (split-sequence
                                                                 " "
                                                                     (first 
                                                                      insts))
                                                                      :test #'equal))
                                               (getValue insts2 
                                                     insts2 
                                                     (second 
                                                     (remove "" (split-sequence 
                                                        " "
                                                        (first 
                                                            insts)) 
                                                        :test #'equal))) nil))
                                             :junk-allowed T))
                (translate (rest insts) insts2))
             (if (isLabel inst)
                (cons (write-to-string (parse-integer (concatenate 'string 
                                                        (getOp 
                                                        (second (remove "" 
                                                                (split-sequence 
                                                                " "
                                                                (first insts))
                                                        :test #'equal)))
                                                        (getValue insts2 
                                                             insts2 
                                                             (third 
                                                                (remove ""
                                                                (split-sequence
                                                                " " 
                                                                (first insts))
                                                                :test #'equal))))
                                                        :junk-allowed T))
                                                  (translate 
                                                    (rest insts) insts2))
                            nil))) nil))

(defun to-mem (x)
        (if (eq (length x) 100)
            x 
            (to-mem (append x '("0")))))

(defun clean-mem (x)
    (if x
        (cons (parse-integer (first x) :junk-allowed T) (clean-mem (rest x)))
        nil))

(defun raw-mem (x)
    (if x
        (cons (write-to-string (first x)) (raw-mem (rest x)))
        nil))

(defun lmc-load (file) (let ((lines (remove "" (getCleanLines 
                                                    (with-open-file 
                                                        (stream 
                                                        file 
                                                        :direction :input)
                                                  (read-lines stream)))
                                                  :test #'equal)))
                            (clean-mem (to-mem (translate lines lines)))))

(defun getComponents (inst)
          (list (subseq inst 0 1)
                (if (eq (length (subseq inst 1 (length inst))) 0)
                    nil
                    (subseq inst 1 (length inst)))))

(defun replace-elem ( a n l )
    (if l
        (if (zerop n)
            (cons a (cdr l))
            (cons (car l) (replace-elem a (1- n) (cdr l))))))

(defun add (arg st)
        (let ((roba (getf (rest st) :acc)))
        ;(write roba)
        (list   'state
                :acc (mod (+ (getf (rest st) :acc) 
                        (parse-integer (nth arg (getf (rest st) :mem)))) 1000)
                :pc (mod (+ (getf (rest st) :pc) 1) 1000)
                :mem (getf (rest st) :mem)
                :in (getf (rest st) :in)
                :out (getf (rest st) :out)
                :flag (if (> (+ (getf (rest st) :acc) 
                        (parse-integer (nth arg (getf (rest st) :mem)))) 
                                999) 'flag 'noflag))))

(defun sub (arg st)
        (let ((roba (getf (rest st) :acc)))
        ;(write roba)
        (list   'state
                :acc (mod (- (getf (rest st) :acc) 
                        (parse-integer (nth arg (getf (rest st) :mem)))) 1000)
                :pc (mod (+ (getf (rest st) :pc) 1) 1000)
                :mem (getf (rest st) :mem)
                :in (getf (rest st) :in)
                :out (getf (rest st) :out)
                :flag (if (<  (- (getf (rest st) :acc) 
                        (parse-integer (nth arg (getf (rest st) :mem))))
                                0) 'flag 'noflag))))

(defun store (arg st)
        (list   'state
                :acc (getf (rest st) :acc)
                :pc (mod (+ (getf (rest st) :pc) 1) 1000)
                :mem (replace-elem (write-to-string (getf (rest st) :acc)) 
                        arg (getf (rest st) :mem))
                :in (getf (rest st) :in)
                :out (getf (rest st) :out)
                :flag (getf (rest st) :flag)))

(defun load_ (arg st)
        (list   'state
                :acc (if (>= arg 0)
                        (parse-integer (nth arg (getf (rest st) :mem)))
                        nil)
                :pc (mod (+ (getf (rest st) :pc) 1) 1000)
                :mem (getf (rest st) :mem)
                :in (getf (rest st) :in)
                :out (getf (rest st) :out)
                :flag (getf (rest st) :flag)))

(defun branch (arg st)
        (list   'state
                :acc (getf (rest st) :acc)
                :pc arg
                :mem (getf (rest st) :mem)
                :in (getf (rest st) :in)
                :out (getf (rest st) :out)
                :flag (getf (rest st) :flag)))

(defun branchiz (arg st)
        (list   'state
                :acc (getf (rest st) :acc)
                :pc (if (and (eq (getf (rest st) :flag) 'noflag)
                             (eq (getf (rest st) :acc) 0))
                            arg
                                (mod (+ (getf (rest st) :pc) 1) 1000))
                :mem (getf (rest st) :mem)
                :in (getf (rest st) :in)
                :out (getf (rest st) :out)
                :flag (getf (rest st) :flag)))

(defun branchip (arg st)
        (list   'state
                :acc (getf (rest st) :acc)
                :pc (if (eq (getf (rest st) :flag) 'noflag)
                             arg
                            (mod (+ (getf (rest st) :pc) 1) 1000))
                :mem (getf (rest st) :mem)
                :in (getf (rest st) :in)
                :out (getf (rest st) :out)
                :flag (getf (rest st) :flag)))

;;;ACC NULL IF ERR
(defun input (st)
        (list   'state
                :acc (if (getf (rest st) :in)
                        (first (getf (rest st) :in))
                        nil)
                :pc (mod (+ (getf (rest st) :pc) 1) 1000)
                :mem (getf (rest st) :mem)
                :in (if (getf (rest st) :in) (rest (getf (rest st) :in)) 
                        (getf (rest st) :in))
                :out (getf (rest st) :out)
                :flag (getf (rest st) :flag)))

(defun output (st)
        (list   'state
                :acc (getf (rest st) :acc)
                :pc (mod (+ (getf (rest st) :pc) 1) 1000)
                :mem (getf (rest st) :mem)
                :in (getf (rest st) :in)
                :out (append (getf (rest st) :out) (list (getf (rest st) :acc)))
                :flag (getf (rest st) :flag)))

(defun halt (st)
        (list   'halted_state
                :acc (getf (rest st) :acc)
                :pc (getf (rest st) :pc)
                :mem (getf (rest st) :mem)
                :in (getf (rest st) :in)
                :out (getf (rest st) :out)
                :flag (getf (rest st) :flag)))

(defun process-state (state)
      (if (ok-state state)
        (let ((inst (getcomponents (nth (getf (rest state) :pc)
                    (getf (rest state) :mem)))))
                ;(write state)
                (cond ((string= (first inst) "1") 
                    (add (parse-integer (second inst)) state))

                  ((string= (first inst) "2")
                    (sub (parse-integer (second inst)) state))

                  ((string= (first inst) "3")
                    (store (parse-integer (second inst)) state))

                  ((string= (first inst) "5")
                    (load_ (parse-integer (second inst)) state))

                  ((string= (first inst) "6")
                    (branch (parse-integer (second inst)) state))

                  ((string= (first inst) "7")
                    (branchiz (parse-integer (second inst)) state))

                  ((string= (first inst) "8")
                    (branchip (parse-integer (second inst)) state))

                  ((equal inst '("9" "01"))
                    (input state))

                  ((equal inst '("9" "02"))
                    (output state))

                  ((string= (first inst) "0")
                    (halt state))
                  (inst (and (write "ERR: ") (write inst)))
                    )) (ok-state state)))

(defun clean-state (st)
    (list   (first st)
            :acc (getf (rest st) :acc)
            :pc (getf (rest st) :pc)
            :mem (clean-mem (getf (rest st) :mem))
            :in (getf (rest st) :in)
            :out (getf (rest st) :out)
            :flag (getf (rest st) :flag)))

(defun raw-state (st)
    (list   (first st)
            :acc (getf (rest st) :acc)
            :pc (getf (rest st) :pc)
            :mem (raw-mem (getf (rest st) :mem))
            :in (getf (rest st) :in)
            :out (getf (rest st) :out)
            :flag (getf (rest st) :flag)))


(defun process-loop (state)
    (if (and (ok-state state) T)
        (if (string= (first (getcomponents (nth (getf (rest state) :pc)
                        (getf (rest state) :mem)))) "0")
                (process-state state)
                (if (eq (first state) 'state) 
                    (process-loop (process-state state))
                    (state))) (ok-state state))) 

(defun execution-loop (state)
    (getf (rest (clean-state (process-loop (raw-state state)))) :out))

(defun one-instruction (state) (process-state state))

(defun db-state (file in)
    (list   'state
                            :acc 0
                            :pc 0
                            :mem (lmc-load file)
                            :in in
                            :out nil
                            :flag 'noflag))

(defun ok-state (st)
    (if (eq (first st) 'halted_state) st
    (if (or (eq st nil)
            (< (getf (rest st) :pc) 0)
            (eq (getf (rest st) :acc) nil)
            (eq (getf (rest st) :pc) nil)
            (eq (getf (rest st) :mem) nil)
            (eq (getf (rest st) :flag) nil)
            (> (+ (getf (rest st) :pc) 1) (length (getf (rest st) :mem)))) nil T)))

(defun lmc-run (file in) 
    (execution-loop (list   'state
                            :acc 0
                            :pc 0
                            :mem (lmc-load file)
                            :in in
                            :out nil
                            :flag 'noflag)))