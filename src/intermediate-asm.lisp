(in-package :nass.instruction)

(defstruct variable
  "Description of an intermediate assembly variable."
  address
  size
  name
  value)

(defstruct label
  "Lable used for jump statement navigation."
  address
  name)

(defstruct move-instruction)

;;; any non specialized instruction
(defstruct basic-instruction)

(defstruct jump-instruction)
