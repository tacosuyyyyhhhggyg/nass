(in-package :nass.asm)

(defun keyword->label (input)
  (check-type input (not nass.instruction::label))
  (if (keywordp input)
      (nass.instruction:make-label :name input)
      input))

(defun var (size name value)
  (nass.instruction:make-variable :size size :name name :value value))

(defgeneric valid-register-p (machine input)
  (:documentation "True if INPUT is a valid register on MACHINE."))

(defgeneric lookup-size (machine input)
  (:documentation "Look up size in bytes of INPUT on MACHINE.

This will return nil in the case that INPUT is not a valid size on
MACHINE."))

(defmacro asm ((&key) &body body)
  `(list ,@(mapcar #'keyword->label body)))

#+ () (asm ()
  :hi
  (var 1 :oh #x1)
  (inst op :ax :hi))
