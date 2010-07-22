(in-package :nass.x86oid.opcodes)

(defvar *x86-opcodes* (make-array '(3 256) :initial-element nil)
  "Table of operations indexed by opcode.")

(defvar +x86-mnemonics+
  (make-hash-table :test 'eq))

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
  ((immediate) (list #xCD immediate)))
(defun encode-reg-bits (reg-name)
  "Compute 3 bit number corresponding to REG-NAME."
  (declare (nass.x86oid.types::mod-rem-r/m-register reg-name)
           (optimize (speed 3) (space 0)))
  (mod (position reg-name (the simple-vector +register-list+)) 8))

(defun encode-instruction (name &rest operands)
  (declare (optimize (speed 3) (safety 3)))
  (apply (the function (nass.instruction::instruction-writer (gethash name +x86-mnemonics+)))
         operands))


(defmacro asm ((arch) &body body)
  (ecase arch
    (:x86oid `(list ,@(mapcar (lambda (instruction)
                        `(nass.x86oid.opcodes::encode-instruction ,(nutils:make-keyword (car instruction)) ,@(cdr instruction)))
                      body)))))

