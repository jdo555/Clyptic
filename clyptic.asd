(defsystem "clyptic"
  :author "Jacob Olson"
  :version "1.0.0"
  :license "MIT"
  :description "A Common Lisp implementation of classical cryptographic methods."
  ;;:depends-on
  :components ((:file "clyptic")))

(defsystem "clyptic/tests/general"
  :author "Jacob Olson"
  :license "MIT"
  :description "Tests of all clyptic constants, general functions, and more."
  :depends-on ("rove" "clyptic")
  :components ((:module "tests"
                :components
                ((:file "general")))))

(defsystem "clyptic/tests/ciphers"
  :author "Jacob Olson"
  :license "MIT"
  :description "Tests of all clyptic ciphers."
  :depends-on ("rove" "clyptic")
  :components ((:module "tests"
                :components
                ((:file "ciphers")))))
