(in-package :cl-user)
(defpackage lutra
  (:use :cl)
  (:import-from :cl-mecab
                #:make-mecab
                #:parse*)
  (:import-from :lutra.scenario-storage
                #:load-dialogue
                #:add-dialogue
                #:remove-dialogue)
  (:export #:lutra
           #:vocab-size
           #:match-utterances
           #:utterance-dialogues
           #:all-dialogues
           #:add-dialogue
           #:remove-dialogue))
(in-package :lutra)


(defclass lutra ()
  ((storage :initarg :storage
            :initform (error ":storage is required")
            :reader storage)
   (mecab :initform (make-mecab)
          :reader mecab)
   (idf-table :accessor idf-table)
   (word-index :accessor word-index)
   (utterance-dialogues-table :accessor utterance-dialogues-table)
   (utterance-vectors :initform nil
                      :accessor utterance-vectors)))


(defmethod initialize-instance :after ((lutra lutra) &key storage)
  (build lutra (load-dialogue storage)))


(defmethod vocab-size ((lutra lutra))
  (word-index lutra))


(defmethod doc-words ((lutra lutra) doc)
  (loop
     for morph in (parse* doc (mecab lutra))
     when (string/= (second morph) "記号")
     collect (eighth morph)))

(defmethod build ((lutra lutra) dialogues)
  ;; build idf-table
  (let ((word-count (make-hash-table :test 'equal))
        (idf-table (make-hash-table :test 'equal)))

    (loop
       for (utterance . _) in dialogues
       do (loop
             for word in (doc-words lutra utterance)
             do (incf (gethash word word-count 0))))

    (loop
       with num-dialogues = (length dialogues)
       for word being each hash-key of word-count
       do (setf (gethash word idf-table)
                (+ (log (/ num-dialogues (gethash word word-count)))
                   1)))

    (setf (idf-table lutra) idf-table))

  ;; build word-index
  (let ((word-index (make-hash-table :test 'equal)))
    (loop
       with index = -1
       for word being each hash-key of (idf-table lutra) using (hash-value idf)
       do (setf (gethash word word-index) (incf index)))
    (setf (word-index lutra) word-index))

  ;; build utterance-dialogues-table
  (let ((utterance-dialogues-table (make-hash-table :test 'equal)))
    (loop
       for dialogue in dialogues
       for utterance = (car dialogue)
       do (push dialogue (gethash utterance utterance-dialogues-table nil)))
    (setf (utterance-dialogues-table lutra) utterance-dialogues-table))

  ;; build utterance-vectors
  (setf (utterance-vectors lutra)
        (loop
           for (utterance . _) in dialogues
           collect (cons utterance (utterance-vector lutra utterance)))))

(defmethod utterance-vector ((lutra lutra) utterance)
  (let ((words (doc-words lutra utterance))
        (vector (make-array (hash-table-count (idf-table lutra))
                            :element-type 'float :initial-element 0))
        (word-index (word-index lutra)))
    (loop
       for word in words
       for index = (gethash word word-index)
       when index
       do (incf (aref vector index)))
    (loop
       for word being each hash-key of (idf-table lutra) using (hash-value idf)
       for i from 0
       for tf = (aref vector i)
       do (setf (aref vector i) (* tf idf)))
    #|
    (loop
       for word being each hash-key of (idf-table lutra) using (hash-value idf)
       for i from 0
       for tf = (count word words :test #'string=)
       do (setf (aref vector i) (* tf idf)))|#
    vector))

(defun cosine-similarity (vector1 vector2)
  (loop
     with value = 0.0
     with acc1 = 0.000001
     with acc2 = 0.000001
     for x across vector1
     for y across vector2
     do (incf value (* x y))
       (incf acc1 (* x x))
       (incf acc2 (* y y))
     finally (return (/ value (sqrt acc1) (sqrt acc2)))))

(defmethod match-utterances ((lutra lutra) utterance)
  (let ((vector (utterance-vector lutra utterance)))
    (sort
     (loop
        for (utterance1 . vector1) in (utterance-vectors lutra)
        collect (cons utterance1 (cosine-similarity vector vector1)))
     #'>
     :key #'cdr)))

(defmethod utterance-dialogues ((lutra lutra) utterance)
  (gethash utterance (utterance-dialogues-table lutra)))

(defmethod all-dialogues ((lutra lutra))
  (loop
     for key being each hash-key of (utterance-dialogues-table lutra)
     using (hash-value value)
     append value))

(defmethod add-dialogue ((lutra lutra) dialogue)
  (assert (and (consp dialogue)
               (stringp (car dialogue))
               (stringp (cdr dialogue))))

  ;; duplication check
  (when (member dialogue
                (gethash (car dialogue)
                         (utterance-dialogues-table lutra)
                         nil)
                :test #'equal)
    (return-from add-dialogue nil))

  ;; update storage
  (add-dialogue (storage lutra) dialogue)

  ;; add dialogue
  (let ((all-dialogues nil))
    (maphash (lambda (_ dialogues)
               (setf all-dialogues (append all-dialogues dialogues)))
             (utterance-dialogues-table lutra))
    (push dialogue all-dialogues)
    (build lutra all-dialogues))

  t)

(defmethod remove-dialogue ((lutra lutra) dialogue)
  (assert (and (consp dialogue)
               (stringp (car dialogue))
               (stringp (cdr dialogue))))

  ;; existence check
  (unless (member dialogue
                  (gethash (car dialogue)
                           (utterance-dialogues-table lutra)
                           nil)
                  :test #'equal)
    (return-from remove-dialogue nil))

  ;; update storage
  (remove-dialogue (storage lutra) dialogue)

  ;; add dialogue
  (let ((all-dialogues nil))
    (maphash (lambda (_ dialogues)
               (setf all-dialogues (append all-dialogues dialogues)))
             (utterance-dialogues-table lutra))
    (setf all-dialogues (delete dialogue all-dialogues :test #'equal))
    (build lutra all-dialogues))

  t)
