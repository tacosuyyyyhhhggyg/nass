(in-package :nass.arch.x86oids)

(define-binary-class x86oid (little-endian) ())

(macrolet ((define-type (name full-name width &rest members)
             (declare (symbol name full-name)
                      ((unsigned-byte 1) width))
             (let ((documentation
                    (format nil "Mnemonic ~A stands for ~(~A~).~
                                ~%~%WIDTH must be ~D." name
                                (substitute #\Space #\- (symbol-name full-name))
                                width)))
               `(progn
                  (deftype ,full-name (&key (width ,width))
                    ,documentation
                    (when (= width ,width)
                      '(member ,@members)))
                  (deftype ,name (&key (width ,width))
                    ,documentation
                    (when (= width ,width)
                      ',full-name))))))
  (define-type ax accumulator-register 1 0)
  (define-type bx base-address-register 1 3)
  (define-type cx count-register 1 1)
  (define-type dx data-register 1 2)
  (define-type sp stack-pointer 1 4)
  (define-type bp base-pointer 1 5)
  (define-type si string-index 1 6)
  (define-type di data-index 1 7)
  ;; low and high bits
  (define-type al low-accumulator-register 0 0)
  (define-type bl low-base-address-register 0 3)
  (define-type cl low-count-register 0 1)
  (define-type dl low-data-register 0 2)
  (define-type ah high-accumulator-register 0 4)
  (define-type bh high-base-address-register 0 7)
  (define-type ch high-count-register 0 5)
  (define-type dl high-data-register 0 6))

(define-binary-class mod-reg-r/m (x86oid)
  ((mod :bits 2 :initarg :mod :type (mod 4))
   (r/m :bits 3 :initarg :r/m :type (mod 8))
   (reg/opcode :bits 3 :initarg :reg/opcode :type (mod 8))))

;;; END