(in-package :nass.x86oid.opcodes)

(defvar *x86-opcodes* (make-array '(3 256) :initial-element nil)
  "Table of operations indexed by opcode.")

(defvar +x86-mnemonics+
  (make-hash-table :test 'eq))

