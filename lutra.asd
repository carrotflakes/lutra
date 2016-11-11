#|
  This file is a part of lutra project.
  Copyright (c) 2016 carrotflakes (carrotflakes@gmail.com)
|#

#|
  Author: carrotflakes (carrotflakes@gmail.com)
|#

(in-package :cl-user)
(defpackage lutra-asd
  (:use :cl :asdf))
(in-package :lutra-asd)

(defsystem lutra
  :version "0.1"
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on (:cl-mecab)
  :components ((:module "src"
                :components
                ((:module "scenario-storages"
                  :pathname "scenario-storage"
                  :components
                  ((:file "scenario-storage")
                   (:file "sexp" :depends-on ("scenario-storage"))
                   (:file "journal" :depends-on ("scenario-storage"))))
                 (:file "lutra" :depends-on ("scenario-storages")))))
  :description "A dialogue engine based on pairs of dialogue"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op lutra-test))))
