(in-package :nass.instruction)

(defstruct instruction
  (name (assert-value-supplied) :type symbol)
  (primary-opcode (assert-value-supplied) :type (mod 256))

  (extension nil :type (or null keyword))
  (writer nil :type (or null function))
  (reader nil :type (or null function))
  (documentation nil :type (or null string))
  (categories nil :type list)
  (operands nil :type list)
  ;; need to handle tested, modified, defined, undefined and values.
  (flags nil :type (or list array)))


(defun instruction-operands-count (instruction)
  "Count the number of operands in INSTRUCTION."
  (declare ((or instruction x86-instruction) instruction))
  (length (instruction-operands instruction)))

(defstruct (x86-instruction
             (:include instruction))
  (secondary-opcode nil :type (or null (mod 256)))
  (prefix nil :type (or null (mod 256)))
  (opcode-fields nil :type list)
  (register-opcode-field nil :type (or null (mod 8) (member :r))))


(defmacro defop (name opcode (&rest operand) &body args)
  (let ((docstring (and (stringp (car args))
                        (car args)))
        (remaining-args (and (stringp (car args))
                             (cdr args))))
    `(make-x86-instruction :name ',name
                           :primary-opcode ,opcode
                           :operands ',operand
                           :documentation ,docstring
                           ,@remaining-args)))
#|
(instruction-operands-count (defop foo #x00 ((E f))
   "look a test"
   :categories '(:binary)))

(defop nop #x90 () "No operation.")
(defop nop #x90 () "No operation."
       :prefix #xf3)

(defop nop #x1f () "No operation."
       :register-opcode-field 0
       :prefix #x0f)

(defop nop #x0f () "No operation."
       :prefix #x0d)

(deftype r8 ()
  "8 bit address locations on 16 bit."
  '(member :al :cl :dl :bl :ah :ch :dh :bh))

(deftype r16 ()
  "16 bit registers."
  '(member :ax :cx :dx :bx :sp :bp :si :di))


(deftype r32 ()
  "32 bit registers."
  '(member :eax :ecx :edx :ebx :esp :ebp :esi :edi))

(deftype r64 ()
  "64 bit registers."
  '(member :rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi
    :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15))

(deftype x86-register ()
  '(or r8 r16 r32 r64))
SB-PRETTY::PPRINT-DISPATCH-ENTRY
|#

(defvar +x86-mnemonics+
  (make-hash-table :test 'eq))

(defstruct dispatch-entry
  "Contains required TYPE and PRIORITY that FUNCTION is called.

Meant for use in a hashtable with a list of these matching a particular
function name so that dispatch based on _types_ can happen. By default we
just do `typep' on the function's arguments, but for more specialized
checking the function TEST-FUNCTION can be modified to return t when
FUNCTION should be called.

TEST-FUNCTION is a function of one argument, that of the prospective
arguments to check types on that returns t or nil if those arguements are
of a valid type."
  (type (assert-value-supplied))
  (test-function nil (or null nutils:function-designator))
  (priority 0 :type fixnum)
  (function (assert-value-supplied) :type nutils:function-designator))
