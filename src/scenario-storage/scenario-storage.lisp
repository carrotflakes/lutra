(in-package :cl-user)
(defpackage lutra.scenario-storage
  (:use :cl)
  (:export #:scenario-storage
           #:load-dialogue
           #:add-dialogue
           #:remove-dialogue))
(in-package :lutra.scenario-storage)


(defclass scenario-storage ()
  ())

(defgeneric load-dialogue (scenario-storage))

(defgeneric add-dialogue (scenario-storage cons))

(defgeneric remove-dialogue (scenario-storage cons))
