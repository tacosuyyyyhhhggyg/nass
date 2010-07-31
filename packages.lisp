
(defpackage #:nass.types
  (:use :cl :eos :nutils)
  (:nicknames :nass-type)
  (:export #:nibble
           #:octet
           #:word
           #:unicode-point
           #:octal-digit
           #:double-word
           #:mips-word
           #:hexadecimal-digit
           #:signed-nibble
           #:signed-octet
           #:signed-word
           #:signed-double-word
           #:signed-mips-word
           #:endian
           #:architectures))

(defpackage #:nass.types.mips
  (:use :cl)
  (:nicknames :mips-type)
  (:import-from :nass.types
                :octet)
  (:export #:halfword
           #:word
           #:doubleword))
(defpackage #:nass.ctypes
  (:use :cl)
  (:import-from :nass.types #:octet)
  (:shadow #:char #:float)
  (:documentation "Types as understood by the gnu compiler.

These are descriptions of data sizes spoken about often in many assembly,
C, C++, and osdev manuals. We define these in this package so there is no
ambigouity from C types to our lisp description."))

#+ ()
(defpackage #:nass.convert
  (:use :cl :nutils :convert))

(defpackage #:nass.util
  (:use :cl :eos :nutils)
  (:export #:write-binary-file
           #:with-hex))

(defpackage #:nass.arch.amd64
  (:use :cl :nass.util :eos))

(defpackage #:nass.global-state
  (:use :cl)
  (:documentation "Dynamic variables indicating assembler state.")
  (:export #:*machine-size*
           #:*architecture*
           #:*instruction-set*))

(defpackage #:nass.abstract
  (:use :cl :nutils :binary-data)
  (:export #:operands
           #:operand-mixin
           #:opcode
           #:opcode-mixin
           #:define-assembly-class))

(defpackage #:nass.instruction
  (:use :cl :nutils.assert)
  (:shadow #:variable)
  (:export #:make-label
           #:make-variable))

(defpackage #:nass.x86oid.types
  (:use :cl)
  (:export #:r8
           #:r16
           #:r32
           #:r64
           #:mm
           #:xmm
           #:segment-register
           #:eee
           #:x87-stack-register
           #:mod-rem-r/m-register
           #:segment-override-prefix-codes
           #:x86-register
           #:immediate
           #:immediate-octet
           #:indirect-base-displacement
           #:indirect-base
           #:indirect-displacement
           #:register-indirect
           #:displacement
           #:segment
           #:memory
           #:gpr
           #:general-purpose-register
           #:register-indirect-register
           #:indirect-displacement-location
           #:indirect-base-displacement-location
           #:indirect-base-base
           #:make-displacement
           #:make-register-indirect
           #:make-indirect-base
           #:make-indirect-displacement
           #:make-indirect-base-displacement
           #:displacement-location
           #:gpr-or-memory
           #:immediate-xoctet))
(defpackage #:nass.arch.x86oids
  (:use :cl :nutils :eos :binary-data :nass.abstract
        :nass.global-state)
  (:nicknames #:n-x86oid)
  (:documentation "")
  (:shadow #:push #:pop)
  (:export #:x86oid
           #:mod-reg-r/m))

(defpackage #:nass.arch.x86
  (:use :cl :nass.util :eos))

(defpackage #:nass.arch.i8086
  (:use :cl :nass.util :binary-data :nass.arch.x86oids))

(defpackage #:nass.arch.4004
  (:use :cl :nass.util :nutils :eos)
  (:documentation "Really old processor, this is mostly for goofing off
  and learning a bit."))

(defpackage #:nass.arch.arm
  (:use :cl :nutils)
  (:documentation "Working with the ARM manual. Nothing concrete at all
  here yet and may not be for several months. [2010-05-21 Fri 12:59]"))

(defpackage #:nass.x86oid.opcodes
  (:use :cl :nass.x86oid.types))

(defpackage #:nass.elf
  (:use :cl :nass.util :eos))

(defpackage #:nass.asm
  (:use :cl :nass.instruction)
  (:export #:asm))

(defpackage #:nass.goof
  (:use :cl :eos :nutils :binary-data))

(defpackage #:nass.general
  (:use :cl :nutils :binary-data))


;;; END
