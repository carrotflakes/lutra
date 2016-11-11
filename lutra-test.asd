#|
  This file is a part of lutra project.
  Copyright (c) 2016 carrotflakes (carrotflakes@gmail.com)
|#

(in-package :cl-user)
(defpackage lutra-test-asd
  (:use :cl :asdf))
(in-package :lutra-test-asd)

(defsystem lutra-test
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on (:lutra
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "lutra"))))
  :description "Test system for lutra"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
