(in-package :nass.instruction)

(defstruct instruction
  (name (assert-value-supplied) :type symbol)
  (primary-opcode 0 :type (mod 256))

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




(defun build-cons-type (list)
  (if list
      (if (eql 'null (car list))
          'null
          (list 'cons (car list) (build-cons-type (cdr list))))
      '*))


(defmacro define-type-dispatch (lambdalist &body types-actions)
  (let ((args (gensym "ARGS")))
   `(lambda (&rest ,args)
      (destructuring-bind ,lambdalist ,args
        (etypecase ,args
          ,@(mapcar (lambda (arg)
                      (if (consp (car arg))
                        (cons (build-cons-type (car arg))
                              (cdr arg))
                        arg))
                    types-actions))))))


