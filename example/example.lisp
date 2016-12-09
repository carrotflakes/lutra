(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :lutra))

(use-package :lutra)


(defvar *lutra*
  (make-instance
   'lutra
   :storage (make-instance
             'lutra.scenario-storage.sexp:sexp-storage
             :file-path "scenario.txt")))


(defun converse (utterance)
  (let ((utterance-scores (match-utterances *lutra* utterance)))
    (when utterance-scores
      (setf utterance-scores
            (remove (- (cdr (first utterance-scores)) 0.01)
                    utterance-scores
                    :key #'cdr
                    :test #'>))
      (subseq utterance-scores 0 (min 10 (length utterance-scores)))
      (let ((dialogues
             (utterance-dialogues *lutra*
                                  (car (elt utterance-scores
                                            (random (length utterance-scores)))))))
        (cdr (elt dialogues (random (length dialogues))))))))

(loop
   for user-utterance = (progn (princ "user> ") (force-output) (read-line))
   do (format t "bot> ~a~%" (converse user-utterance)))
