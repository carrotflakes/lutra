(in-package :cl-user)
(defpackage lutra.scenario-storage.sexp
  (:use :cl)
  (:import-from :lutra.scenario-storage
                #:scenario-storage
                #:load-dialogue
                #:add-dialogue
                #:remove-dialogue)
  (:export #:sexp-storage
           #:load-dialogue
           #:add-dialogue
           #:remove-dialogue))
(in-package :lutra.scenario-storage.sexp)


(defclass sexp-storage (scenario-storage)
  ((file-path :initarg :file-path
              :initform (error ":file-path is required")
              :reader file-path)
   (dialogues :initform nil
              :accessor dialogues)))

(defmethod load-dialogue ((sexp-storage sexp-storage))
  (with-open-file (stream (file-path sexp-storage)
                          :direction :input
                          :if-does-not-exist :create)
    (setf (dialogues sexp-storage)
          (read stream nil nil))))

(defmethod add-dialogue ((sexp-storage sexp-storage) dialogue)
  (push dialogue (dialogues sexp-storage))

  (with-open-file (stream (file-path sexp-storage)
                          :direction :output
                          :if-exists :supersede)
    (prin1 (dialogues sexp-storage) stream)))

(defmethod remove-dialogue ((sexp-storage sexp-storage) dialogue)
  (setf (dialogues sexp-storage)
        (delete dialogue (dialogues sexp-storage) :test #'equal))

  (with-open-file (stream (file-path sexp-storage)
                          :direction :output
                          :if-exists :supersede)
    (prin1 (dialogues sexp-storage) stream)))
