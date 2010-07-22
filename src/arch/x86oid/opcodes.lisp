(in-package :nass.x86oid.opcodes)

(defvar *x86-opcodes* (make-array '(3 256) :initial-element nil)
  "Table of operations indexed by opcode.")

(defvar +x86-mnemonics+
  (make-hash-table :test 'eq))

(defun integer->little-octets (integer &key vectorp
                               (size (ceiling (log (1+ integer) 256))))
  "Convert an INTEGER to a list of octets in little endian order."
  (assert (not vectorp) nil "VECTORP not implemented yet.")
  (loop for i from 0 to (1- size)
       collect (ldb (byte 8 (* 8 i)) integer)))

;;; OPTIMIZE: If anyone cares or thinks this is too slow: Put these in the
;;; order of expected use. The items closer to the front of this array
;;; will be looked up faster then the items at the end. So put the more
;;; common instances at the front, however! do not change the order of the
;;; rows themselves.
(defparameter +register-list+
  #(:al :cl :dl :bl :ah :ch :dh :bh     ;16 and 32 bit regs
    :ax :cx :dx :bx :sp :bp :si :di
    :eax :ecx :edx :ebx :esp :ebp :esi :edi
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7
    :es :cs :ss :ds :fs :gs :reserved :reserved
    :cr0 :invalid :cr2 :cr3 :cr4 :invalid :invalid :invalid
    :dr0 :dr1 :dr2 :dr3 :dr4 :dr5 :dr6 :dr7
    ;; x87 FPU
    :st0 :st1 :st2 :st3 :st4 :st5 :st6 :st7
    ;; These below apply only for amd64 arch.
    :r8b :r9b :r10b :r11b :r12b :r13b :r14b :r15b
    :r8w :r9w :r10w :r11w :r12w :r13w :r14w :r15w
    :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d
    :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :XMM8 :XMM9 :XMM10 :XMM11 :XMM12 :XMM13 :XMM14 :XMM15
    :cr8 :invalid :invalid :invalid :invalid :invalid :invalid :invalid)
  "table of all valid x86oid registers.

These are in order such that:
  (mod (position keyword `+register-list+') 8)
will return the correct bit sequence as an integer.")


(defmacro define-x86oid-mnemonic (name lambdalist &body types-action)
  `(setf (gethash ,(nutils:make-keyword name) +x86-mnemonics+)
        #+ () (nass.instruction::define-type-dispatch
                  ,lambdalist ,@types-action)
         (nass.instruction::make-instruction
          :name ',name
          :writer (nass.instruction::define-type-dispatch
                      ,lambdalist ,@types-action)
          )))

;;;
;;; syntax sorting stuff...
;;;
(defun spec-type-v-p (operands)
  "True if an operand has :v specified.

:v means there is an operand size prefix and it toggles between r16 and
r32 for 32bit mode."
  (declare (list operands))
  (find :v operands :key #'cdr))

;;; mod-reg-r/m stuff
(defun reg-reg (destination source)
  (declare (mod-rem-r/m-register destination source))
  (logior #xC0
          (ash (encode-reg-bits source) 3)
          (encode-reg-bits destination)))

(defun encode-reg-bits (reg-name)
  "Compute 3 bit number corresponding to REG-NAME."
  (declare (nass.x86oid.types::mod-rem-r/m-register reg-name)
           (optimize (speed 3) (space 0)))
  (mod (position reg-name (the simple-vector +register-list+)) 8))

(defun encode-displacement (displacement size)
  "Compute x86 DISPLACEMENT of SIZE.

Doing this means reversing the order of the octets.
   #xFF01 => #x01FF."
  (declare ((nass.types:octet 4) displacement)
           ((member 8 16 32) size))
  (let ((size (1- (ash size -3))))
    (loop for i from size downto 0
       for opp from 0 to size
       collecting (ldb (byte 8 (* i 8)) displacement)
;       do (print (list i opp (* i 8) (ldb (byte 4 (* i 8)) displacement)
 ;                      (ash (ldb (byte 8 (* i 8)) displacement) (* opp 8))))
       )))

(defun encode-reg-r/m (destination source &optional (size 16))
  "Create the encode-reg-r/m byte for a 16 bit machine.

This is very hackish and really needs to be redone when the generic type
dispatch system is complete. -- Nixeagle [2010-07-22 Thu 01:44]"
  (declare (optimize (speed 3) (safety 1)))
  ;; Basically as is each "case" for a 16 bit machine is handled vie types
  ;; alone. This makes this one function very long and ugly... but right
  ;; now required if we are to avoid using clos in the assembler.
  (etypecase (cons destination source)
    ((cons mod-rem-r/m-register mod-rem-r/m-register)
     ;; Example:: ADD ax, ax
     (reg-reg destination source))
    ((cons mod-rem-r/m-register displacement)
     (cons (logior #b00110000 (encode-reg-bits destination))
           (encode-displacement (displacement-location source) size)))
    ((cons (or r8 r16 r32 mm xmm eee segment-register) register-indirect)
     (logior #x00
             (ash (encode-reg-bits destination) 3)
             (+ 4 (position (register-indirect-register source)
                            #(:si :di :bp :bx)))))
    ((cons (or r8 r16 r32 mm xmm eee segment-register)
           indirect-displacement)
     ;; example:: ADD [bx+1), al
     (logior
      (if (> (indirect-displacement-location source) #xFF)
          #b10000000
          #b01000000)
      (ash (encode-reg-bits destination) 3)
      (+ 4 (position (register-indirect-register source)
                     #(:si :di :bp :bx)))))
    ((cons (or r8 r16 r32 mm xmm eee segment-register) indirect-base)
     (logior
      (ash (encode-reg-bits destination) 3)
      (the fixnum (ash (position (the (member :si :di) (register-indirect-register source))
                                 '(:si :di))
                       (position (the (member :bx :bp) (indirect-base-base source))
                                 '(:bx :bp))))))
    ((cons (or r8 r16 r32 mm xmm eee segment-register)
           indirect-base-displacement)
     (logior
      (if (> (indirect-base-displacement-location source) #xFF)
          #b10000000
          #b01000000)
      (ash (encode-reg-bits destination) 3)
      (the fixnum (ash (position (the (member :si :di) (register-indirect-register source))
                                 '(:si :di))
                       (position (the (member :bx :bp) (indirect-base-base source))
                                 '(:bx :bp))))))
    ((cons (or indirect-base displacement
               indirect-displacement register-indirect
               indirect-base-displacement))
     (encode-reg-r/m source destination size))))

;;; mnemonics
(define-x86oid-mnemonic nop ()
  (null (list #x90)))

(define-x86oid-mnemonic dec (op1)
  ((r16) (list (logior #x48 (encode-reg-bits op1))))
  ((r32) (list #x66 (logior #x48 (encode-reg-bits op1)))))

(define-x86oid-mnemonic inc (op1)
  ((r16) (list (logior #x40 (encode-reg-bits op1))))
  ((r32) (list #x66 (logior #x40 (encode-reg-bits op1)))))

(define-x86oid-mnemonic push (op1)
  ((r16) (list (logior #x50 (encode-reg-bits op1))))
  ((r32) (list #x66 (logior #x50 (encode-reg-bits op1)))))

(define-x86oid-mnemonic pop (op1)
  ((r16) (list (logior #x58 (encode-reg-bits op1))))
  ((r32) (list #x66 (logior #x58 (encode-reg-bits op1)))))

(define-x86oid-mnemonic int (immediate)
  ((immediate-octet) (list #xCD immediate)))

(declaim (inline fl))
(defun fl (&rest args)
  "(F)latten (L)ist of ARGS.

Shortcut function that is inlined as we use this often."
  (declare (optimize (speed 3) (safety 0)))
  (nutils:flatten args))

(define-x86oid-mnemonic add (destination source)
  (((member :al) immediate-octet) (list #x04 source))
  (((or memory r8) r8)
   (fl #x00 (encode-reg-r/m source destination)))
  (((or memory r16) r16)
   (fl #x01 (encode-reg-r/m source destination)))
  (((or memory r32) r32)
   (fl #x66 #x01 (encode-reg-r/m source destination)))
  ((r8 (or memory r8))
   (fl #x02 (encode-reg-r/m destination source)))
  ((r16 (or memory r16))
   (fl #x03 (encode-reg-r/m destination source)))
  ((r32 (or memory r32))
   (fl #x66 #x03 (encode-reg-r/m destination source))))

(define-x86oid-mnemonic lodsb ()
  ;; Loads the string at ds:si into al
  (null (list #xac)))
(define-x86oid-mnemonic lodsw ()
  (null (list #xad)))
(define-x86oid-mnemonic lodsd ()
  (null (list #x66 #xAD)))


(define-x86oid-mnemonic aaa ()
  ;; invalid in 64bit mode
  (null #x37))

(defun encode-instruction (name &rest operands)
  (declare (optimize (speed 3) (safety 0))
           (dynamic-extent operands))
  (apply (the function (nass.instruction::instruction-writer (gethash name +x86-mnemonics+)))
         operands))


(defmacro asm ((arch) &body body)
  (ecase arch
    (:x86oid `(list ,@(mapcar (lambda (instruction)
                        `(nass.x86oid.opcodes::encode-instruction ,(nutils:make-keyword (car instruction)) ,@(cdr instruction)))
                      body)))))

