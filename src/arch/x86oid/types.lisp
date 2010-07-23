(in-package :nass.x86oid.types)

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

(deftype mm (&optional criteria)
  (declare (ignore criteria))
  '(member :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7))

(deftype xmm (&optional criteria)
  (declare (ignore criteria))
  '(member :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7))

(deftype segment-register (&optional criteria)
  (declare (ignore criteria))
  "sreg: Segment register names."
  '(member :es :cs :ss :ds :fs :gs))

(deftype eee ()
  '(member :cr0 :cr2 :cr3 :cr4 :dr0 :dr1 :dr2 :dr3 :dr4 :dr5 :dr6 :dr7))

(deftype x87-stack-register ()
  "x87 operates with a stack, not directly addressable registers."
  '(member :st0 :st1 :st2 :st3 :st4 :st5 :st6 :st7))

(deftype mod-rem-r/m-register (&optional criteria)
  "All valid x86oid register names."
  `(or (x86-register ,criteria) mm xmm segment-register eee x87-stack-register
    ;; FIXME: These below should become their own subtypes
    (member :r8b :r9b :r10b :r11b :r12b :r13b :r14b :r15b
     :r8w :r9w :r10w :r11w :r12w :r13w :r14w :r15w
     :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d
     :XMM8 :XMM9 :XMM10 :XMM11 :XMM12 :XMM13 :XMM14 :XMM15
     :cr8)))

(defstruct segment
  (segment :ds :type segment-register))

(defstruct (displacement
             (:constructor make-displacement
                           (location &optional (segment :ds)))
             (:include segment))
  (location 0 :type (mod 65536)))

(defstruct (register-indirect
             (:constructor make-register-indirect
                           (register &optional (segment :ds)))
             (:include segment))
  (register :bx :type (member :bx :bp :si :di
                              :ebx :ebp :esi :edi)))

(defstruct (indirect-displacement
             (:include register-indirect)
             (:constructor make-indirect-displacement
                           (location &optional (register :bx) (segment :ds))))
  ;; have to manually do displacement...
  (location 0 :type (mod 65536)))

(defstruct (indirect-base
             (:include register-indirect)
             (:constructor make-indirect-base
                           (register &optional (base :bx) (segment :ds))))
  (base :bx :type (member :bx :bp :ebx :ebp)))

(defstruct (indirect-base-displacement
             (:include indirect-base)
             (:constructor make-indirect-base-displacement
                           (location &optional (base :bx) (register :si)
                                     (segment :ds))))
  ;; have to manually do displacement
  (location 0 :type (mod 65536)))

(deftype segment-override-prefix-codes ()
  "Octets that override to a specific segment:

Segment names are cs ss ds es fs gs."
  '(member #x2e #x36 #x3e #x26 #x64 #x65))

(deftype opcode-prefix-codes ()
  "Octets that can appear before the primary opcode."
  '(or segment-override-prefix-codes (member #x66 #x67)))

(deftype x86-register (&optional criteria)
  (case criteria
    (8 'r8)
    (16 'r16)
    (32 'r16)
    (64 'r64)
    (otherwise '(or r8 r16 r32 r64))))

(deftype immediate ()
  '(and (not (or symbol keyword))))

(deftype immediate-octet (&optional (size 1))
  `(and immediate (nutils:octet ,size)))

(deftype immediate-xoctet (&optional (size 1))
  `(and immediate (or (nutils:octet ,size) (nutils:signed-octet ,size))))

(deftype general-purpose-register (&optional criteria)
  "Any valid general purpose register."
  `(mod-rem-r/m-register ,criteria))

(deftype gpr (&optional criteria)
  "Short for `general-purpose-register'."
  `(general-purpose-register ,criteria))

(deftype memory (&optional criteria)
  "valid memory locations only."
  (declare (ignore criteria))
  '(or displacement indirect-displacement indirect-base-displacement
    register-indirect indirect-base))

(deftype gpr-or-memory (&optional criteria)
  `(or (gpr ,criteria) (memory ,criteria)))
;;; END
