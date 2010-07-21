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
  (test-function nil :type (or null nutils:function-designator))
  (priority 0 :type fixnum)
  (function (assert-value-supplied) :type nutils:function-designator))

(defun dispatch-entry-equal (entry1 entry2)
  "Compare two entries based on required type alone."
  (declare (dispatch-entry entry1 entry2))
  (equal (dispatch-entry-type entry1)
         (dispatch-entry-type entry2)))


(defun sortf-type-dispatch-entries (name dispatch-table)
  "Sort functions under NAME in DISPATCH-TABLE by priority."
  (setf (gethash name dispatch-table)
        (sort (gethash name dispatch-table)
              #'> :key #'dispatch-entry-priority)))

(defun tuncall (name table &rest args)
  (declare (optimize (speed 3) (safety 0))
           (dynamic-extent args))
  (apply (the function (loop for entry in (gethash name table)
                          when (typep args (dispatch-entry-type entry))
                          return (dispatch-entry-function entry)))
         args))

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


