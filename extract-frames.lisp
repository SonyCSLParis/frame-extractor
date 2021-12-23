;; Copyright Sony Computer Science Laboratories Paris
;; Author: Remi van Trijp (http://www.remivantrijp.eu)

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

(in-package :fcg)

(export '(extract-semantic-frames))

(defgeneric extract-semantic-frames (utterance key &key cxn-inventory &allow-other-keys))

;; Helper functions:
;; -----------------
(defun format-units-as-string (unit-names units)
  "String-append the string associated with a unit, separating them by a white space."
  (format nil "~{~a~^ ~}"
          (mapcar #'(lambda(unit-name)
                      (retrieve-string-for-unit unit-name units))
                  unit-names)))

(defun apply-procedural-attachment (s-expression units)
  "Some procedure needs to be executed before fetching the strings."
  (let ((procedure (first s-expression))
        (body (loop for x in (rest s-expression)
                    collect (if (symbolp x) x (apply-procedural-attachment x units)))))
    (case procedure
      (unit-append (format-units-as-string body units))
      (unit-remove (format-units-as-string (remove (first body) (second body)) units))
      (t
       s-expression))))

;; 1/ PHRASE-BASED:
;; Method that only cares about the strings. This relies on the use of BOUNDARIES.
;; ----------------------------------------------------------------------------------------------------------------------------
(defun lexical-boundary-p (boundary)
  (= 1 (- (third boundary) (second boundary))))

(defun retrieve-string-for-unit (unit-name units)
  (cond ((variable-p unit-name) "UNK")
        ((listp unit-name) (apply-procedural-attachment unit-name units))
        ((stringp unit-name) unit-name)
        (t
         (let* ((boundaries (fcg-get-boundaries units))
                (boundary-spec (assoc unit-name boundaries))
                (strings-in-root (extract-string (get-root units)))
                (lexical-boundaries (loop for boundary in boundaries
                                          when (and (= 1 (- (third boundary) (second boundary)))
                                                    (>= (second boundary) (second boundary-spec))
                                                    (<= (third boundary) (third boundary-spec)))
                                          collect boundary))
                (strings (loop for boundary in lexical-boundaries
                               for string-spec = (let ((strng (find (first boundary) strings-in-root :key #'second)))
                                                   (if strng (list strng)
                                                     (extract-string (assoc (first boundary) units))))
                               when string-spec
                               append (mapcar #'third string-spec))))
           (format nil "~{~a~^ ~}" strings)))))

(defmethod extract-semantic-frames ((utterance string)
                                    (key (eql :phrase-based))
                                    &key cxn-inventory
                                    frame-dev-monitor &allow-other-keys)
  (declare (ignore key))
  (multiple-value-bind (meaning final-node)
      (comprehend utterance :cxn-inventory (or cxn-inventory *fcg-constructions*))
    (declare (ignore meaning))
    (let* (;; 1. Get the feature name that is used for representing semantic frames.  
           (frame-feature (get-frame-feature (or cxn-inventory *fcg-constructions*)))
           (transient-units (fcg-get-transient-unit-structure final-node))
           ;; 2. Retrieve the frames that are evoked in these features and instantiate them.
           (evoked-frames (loop for unit in transient-units
                                for sem-frame = (unit-feature-value unit frame-feature)
                                when sem-frame
                                collect (cons (unit-name unit) sem-frame)))
           (sem-frames (loop for evoked-frame in evoked-frames
                             for frame-type = (unit-feature-value evoked-frame 'frame-type)
                             for frame-elements = (loop for feature in (unit-body evoked-frame)
                                                        unless (eql 'frame-type (feature-name feature))
                                                        collect feature)
                             when (get-frame-def frame-type)
                             collect (make-instance frame-type
                                                    :evoked-by (retrieve-string-for-unit (unit-name evoked-frame) transient-units)
                                                    :frame-elements (loop for fe in frame-elements
                                                                          collect (list (first fe)
                                                                                        (retrieve-string-for-unit (second fe) transient-units)))))))
      ;; To be replaced by a real web monitor.
      (when frame-dev-monitor
        (loop for sem-frame in sem-frames
              do (add-element (make-html sem-frame :expand-initially t))))
      sem-frames)))
;(extract-semantic-frames "The wind caused damage." :phrase-based :cxn-inventory *fcg-english* :frame-dev-monitor t) 

;; 2/ MEANING-BASED:
;; Method for grammars that assume that semantic frames are represented as part of the meaning of the utterance.
;; ----------------------------------------------------------------------------------------------------------------------------
(defun find-unit-with-predicate (predicate units)
  (loop for unit in units
        when (find predicate (unit-feature-value unit 'meaning) :test  #'equal)
        return unit))

(defmethod extract-semantic-frames ((utterance string)
                                    (key (eql :meaning-based))
                                    &key cxn-inventory frame-dev-monitor
                                    &allow-other-keys)
  (declare (ignore key))
  (multiple-value-bind (meaning final-node)
      (comprehend utterance :cxn-inventory (or cxn-inventory *fcg-constructions*))
    (let* ((frame-types (loop for predicate in meaning
                              for frame-elements = (get-frame-elements (first predicate))
                              when frame-elements
                              collect (list predicate frame-elements)))
           (sem-frames (loop for frame-type in frame-types
                             for var = (second (first frame-type))
                             collect (make-instance (first (first frame-type))
                                                    :evoked-by (retrieve-string-for-unit
                                                                (unit-name (find-unit-with-predicate (first frame-type)
                                                                                                     (fcg-get-transient-unit-structure final-node)))
                                                                (fcg-get-transient-unit-structure final-node))
                                                    :frame-elements (loop for fe in (second frame-type)
                                                                          for element = (loop for predicate in meaning
                                                                                              when (and (string= (symbol-name fe) 
                                                                                                                 (symbol-name (first predicate)))
                                                                                                        (eql (second predicate) var))
                                                                                              return predicate)
                                                                          when element
                                                                          collect (list (first element) (third element)))))))
      ;; To be replaced by a real web monitor.
      (when frame-dev-monitor
        (loop for sem-frame in sem-frames
              do (add-element (make-html sem-frame :expand-initially t))))
      sem-frames)))
;; (extract-semantic-frames "The wind caused damage." :meaning-based :cxn-inventory *fcg-english* :frame-dev-monitor t)

;; (comprehend "The wind caused damage." :cxn-inventory *fcg-english*)