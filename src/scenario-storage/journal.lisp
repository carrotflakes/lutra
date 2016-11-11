(in-package :cl-user)
(defpackage lutra.scenario-storage.journal
  (:use :cl)
  (:import-from :lutra.scenario-storage
                #:scenario-storage
                #:load-dialogue
                #:add-dialogue
                #:remove-dialogue)
  (:export #:journal-storage
           #:load-dialogue
           #:add-dialogue
           #:remove-dialogue))
(in-package :lutra.scenario-storage.journal)


(defclass journal-storage (scenario-storage)
  ((file-path :initarg :file-path
              :initform (error ":file-path is required")
              :reader file-path)
   (file-stream)
   (storage :initarg :storage
            :initform (error ":storage is required")
            :reader storage)))


(defmethod initialize-instance :after ((journal-storage journal-storage) &key file-path)
  (setf (slot-value journal-storage 'file-stream)
        (open file-path
              :direction :output
              :if-exists :append
              :if-does-not-exist :create)))


(defmethod load-dialogue ((journal-storage journal-storage))
  (load-dialogue (storage journal-storage)))

(defmethod add-dialogue ((journal-storage journal-storage) dialogue)
  (with-slots (file-stream) journal-storage
    (format file-stream
            "~s~%"
            (cons :add dialogue))
    (force-output file-stream)

    (add-dialogue (storage journal-storage) dialogue)))

(defmethod remove-dialogue ((journal-storage journal-storage) dialogue)
  (with-slots (file-stream) journal-storage
    (format file-stream
            "~s~%"
            (cons :remove dialogue))
    (force-output file-stream)

    (remove-dialogue (storage journal-storage) dialogue)))

; TODO: restore-from
