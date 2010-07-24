(defpackage #:nass.tests.x86oid
  (:use :cl :eos :with-fbound :nass.x86oid.opcodes
        :nass.x86oid.types))

(in-package :nass.tests.x86oid)

(in-suite* :nass)
(in-suite* root :in :nass)

(test (integer->little-octets :suite root)
  (with-fbound (nass.x86oid.opcodes::integer->little-octets)
    (10) => '(10)
    "Reverse of what is expected as this is for writing to disk."
    (256) => '(0 1)
    (0 :size 2) => '(0 0)
    (256 :size 4) => '(0 1 0 0)
    (0 :size 0) :signals simple-type-error
    "Vectorp is not yet implemented, so should error."
    (0 :vectorp t) :signals error ))

(test (encode-reg-bits :suite root)
  (with-fbound (nass.x86oid.opcodes::encode-reg-bits)
    (:ax) => 0
    (:eax) => 0
    (:al) => 0
    (:r15d) => 7
    (1) :signals type-error)
  (is (zerop (mod (length nass.x86oid.opcodes::+register-list+) 8))))

(test (reg-reg :suite root)
  "Should return values between #xC0 and #xFF only"
  (let ((regs (remove :RESERVED
                      (remove :INVALID nass.x86oid.opcodes::+register-list+))))
   (loop repeat 10
      do (let ((reg1 (nutils:random-elt regs))
               (reg2 (nutils:random-elt regs)))
           (is (<= #xC0 (nass.x86oid.opcodes::reg-reg reg1 reg2) #xFF)
               "reg1: ~A reg2: ~A result: ~X"
               reg1 reg2 (nass.x86oid.opcodes::reg-reg reg1 reg2))))))

;;; END
