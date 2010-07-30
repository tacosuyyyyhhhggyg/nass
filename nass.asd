(asdf:defsystem :nass
  :depends-on (:nutils :eos :with-fbound :flexi-streams :binary-data :trivial-shell)
  :serial t
  :components
  ((:file "packages")
   (:file "global")
   (:file "types")
   #+ () (:module :convert
            :components
            ((:file "octets")))
   (:file "util")
   (:module :src
            :serial t
            :components
            ((:file "global-state")
             (:file "c-types")
             (:file "instruction")
            ; (:file "abstract")
             (:module :arch
                      :depends-on ("instruction")
                      :components
                      ((:module :4004
                                :components
                                ((:file "intel-4004")))
                       #+ () (:file "x86oids")
                       (:module :x86oid
                                :components
                                (;(:file "mod-reg-rm-types")
                                 (:file "types")
                                 (:file "opcodes" :depends-on ("types"))
                                 (:file "mnemonic" :depends-on ("opcodes"))))
                       (:module :i8086
                                :components
                                ((:file "init-i8086")))))))
   (:file "nass")
   (:module #:test
            :components
            ((:file "x86-opcodes")))))
