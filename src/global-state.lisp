(in-package :nass.global-state)

(declaim ((integer 2 32) *assembly-unit*))
(defvar *assembly-unit* 8
  "Smallest assembly unit. Most architectures will be 8 bits.

Some arch's require a different size from 8 bits, for example MIX if we
ever assemble for that.

Type is set as anything from 2 to 32, Anything outside of this bound is
highly unusual and should be reported if something like that is actually
in use.
  -- nixeagle [2010-07-24 Sat 17:02]")


(declaim ((integer 2 512) *machine-size*))
(defvar *machine-size* 16
  "Size of machine in bits.

This defaults to 16 bits for now as its the simplest x86 machine size.

Current type restriction is any integer from 2 to 512. Machines are no
bigger then this at this date and this provides good sanity checking.
  -- nixeagle [2010-07-24 Sat 17:03].")

(declaim (keyword *architecture*))
(defvar *architecture* :x86
  "Type of machine.

  - :x86 is all the various i8086 derived things.
  - others might be: :arm :mips :ppc")


(defvar *instruction-set* '(:general)
  "List of additional instruction sets that should be supported. If the
set is not in the list the assembler will not use the instruction set
and will signal an error.")


;;; Assembler things for tracking state during an assembler run. These if
;;; changed are likely to have unintended effects on the assembler itself.

(declaim (nutils:non-negative-integer *program-counter*))
(defvar *program-counter* #x0
  "Program address ")
;;; END

