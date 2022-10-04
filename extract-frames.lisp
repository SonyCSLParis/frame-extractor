;; Copyright Sony Computer Science Laboratories Paris
;; Author:   Remi van Trijp (http://www.remivantrijp.eu)

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

;; ------------------------------------------------------------------------
;; Procedural attachment:
;; ------------------------------------------------------------------------
;; See specific methods at the end of the file.

(defgeneric frame-extractor-procedure (procedure body units))

(defmethod frame-extractor-procedure ((procedure t)
                                      (body list)
                                      (units list))
  (declare (ignore units))
  `(,procedure ,@body))
 
(defun apply-frame-extractor-procedure (s-expression units)
  "Some procedure needs to be executed before fetching the strings."
  (let ((procedure (first s-expression))
        (body (loop for x in (rest s-expression)
                    collect (if (symbolp x) x (apply-frame-extractor-procedure x units)))))
    (frame-extractor-procedure procedure body units)))

;; ------------------------------------------------------------------------
;; Extraction methods:
;; ------------------------------------------------------------------------

;; 1/ PHRASE-BASED:
;; Method that only cares about the strings. This relies on the use of BOUNDARIES and CONSTITUENTS
;; ----------------------------------------------------------------------------------------------------------------------------
(defun lexical-boundary-p (boundary units)
  (if (= 1 (- (third boundary) (second boundary)))
    (let ((unit (assoc (unit-name boundary) units)))
      (not (unit-feature-value unit 'constituents)))
    nil))

(defun retrieve-lexical-boundaries-for-unit (unit-name units)
  (if (listp unit-name) ; Procedural attachhment requested
    (apply-frame-extractor-procedure unit-name units)
    (let* ((boundaries (fcg-get-boundaries units))
           (boundary-spec (assoc unit-name boundaries)))
      (loop for boundary in boundaries
            when (and (lexical-boundary-p boundary units)
                      (>= (second boundary) (second boundary-spec))
                      (<= (third boundary) (third boundary-spec)))
              collect boundary))))

(defun retrieve-string-for-unit (unit-name units)
  (cond ((variable-p unit-name) "UNK")
        ((listp unit-name) (apply-frame-extractor-procedure unit-name units))
        ((stringp unit-name) unit-name)
        (t
         (let* ((strings-in-root (extract-string (get-root units)))
                (lexical-boundaries (retrieve-lexical-boundaries-for-unit unit-name units))
                (strings (loop for boundary in lexical-boundaries
                               for string-spec = (let ((strng (find (first boundary) strings-in-root :key #'second)))
                                                   (if strng (list strng)
                                                     (extract-string (assoc (first boundary) units))))
                               when string-spec
                               append (mapcar #'third string-spec))))
           (format nil "~{~a~^ ~}" strings)))))

(defun filter-evoker (evoker-string evoked-string)
  (let ((l (length evoker-string)))
  (cond
   ((or (not (stringp evoked-string))
        (string= evoked-string "UNK")) evoked-string)
   ((string= evoker-string (subseq evoked-string 0 l))
    (subseq evoked-string (1+ l)))
   ((string= evoker-string (subseq (reverse evoked-string) 0 l))
    (reverse (subseq (reverse evoked-string) (1+ l))))
   (t
    evoked-string))))
; (filter-evoker "because" "UNK")
; (filter-evoker "because" '?test)
; (filter-evoker "because" "because the earth is warming")
; (filter-evoker "because" "the earth is warming because")
; (filter-evoker "because" "the earth is warming")

(defun filter-overlapping-units (evoked-unit-name frame-elements units)
  (let* (;; Always filter the frame-evoking words:
         (evoked-unit (assoc evoked-unit-name units))
         (evoked-unit-names (loop for form-predicate in (unit-feature-value evoked-unit 'form)
                                  when (eql (first form-predicate) 'string)
                                    collect (second form-predicate)))
         ;; Get the word-level units for each frame element without the evoking units:
         (frame-element-units (loop for fe in frame-elements
                                    collect (if (variable-p (second fe))
                                              nil
                                              (remove-if #'(lambda(x)
                                                             (member x evoked-unit-names))
                                                         (mapcar #'first (retrieve-lexical-boundaries-for-unit (second fe) units))))))
         (filtered-frame-element-and-units (loop for role in (mapcar #'first frame-elements)
                                                 for fe-units in frame-element-units
                                                 for subset-units = (loop for other-set in (remove fe-units frame-element-units :test #'equal)
                                                                          when (subsetp other-set fe-units)
                                                                            append other-set)
                                                                              
                                                 collect (list role (loop for unit-name in fe-units
                                                                            unless (member unit-name subset-units)
                                                                            collect unit-name)))))
    (loop for fe in filtered-frame-element-and-units
          collect (list (first fe)
                        (format nil "~{~a~^ ~}" (loop for unit-name in (second fe)
                                                      collect (retrieve-string-for-unit unit-name units)))))))

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
                             collect (let ((evoked-by (retrieve-string-for-unit (unit-name evoked-frame) transient-units)))
                                       (make-instance frame-type
                                                      :evoked-by evoked-by
                                                      :frame-elements (filter-overlapping-units (unit-name evoked-frame)
                                                                                                frame-elements
                                                                                                transient-units))))))
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

;; 3/ DEPENDENCY-BASED:
;; Method for grammars that use dependency parsing for retrieving phrases.
;; ----------------------------------------------------------------------------------------------------------------------------

(defun get-all-dependents (lst units)
  (cond ((null lst) nil)
        ((variable-p (first lst)) (get-all-dependents (rest lst) units))
        (t
         (let* ((unit-name (first lst))
                (full-unit (assoc unit-name units))
                (dependents (unit-feature-value full-unit 'dependents)))
           (cons unit-name
                 (get-all-dependents (append dependents (rest lst)) units))))))

(defgeneric collect-frame-elements-through-dependency-tree 
    (evoking-unit frame-elements units mode))

(defmethod collect-frame-elements-through-dependency-tree ((evoking-unit symbol)
                                                           (frame-elements list)
                                                           (units list)
                                                           (mode t))
  (let* ((frame-element-units (loop for fe in frame-elements
                                    collect `(,(first fe) 
                                              ,(if (variable-p (rest fe))
                                                 "UNK"
                                                 (get-all-dependents 
                                                  (rest fe) units)))))
         (filtered-units 
          (loop for fe-unit in frame-element-units
                collect `(,(first fe-unit)
                          ,(if (stringp (second fe-unit))
                             (second fe-unit)
                             (let ((dependent-subset
                                    (loop for other-fe-unit in frame-element-units
                                          when (and (not (equal fe-unit other-fe-unit))
                                                    (subsetp (second other-fe-unit)
                                                             (second fe-unit)))
                                            append (second other-fe-unit))))
                               (loop for unit-name in (second fe-unit)
                                     unless (or (eql unit-name evoking-unit)
                                                (member unit-name dependent-subset))
                                       collect unit-name))))))
         (boundaries (fcg-get-boundaries units))
         (ordered-units (loop for filtered-unit in filtered-units
                              for role = (first filtered-unit)
                              for units-with-boundaries 
                                = (if (stringp (second filtered-unit))
                                    (second filtered-unit)
                                    (loop for unit-name in (second filtered-unit)
                                          collect (assoc unit-name boundaries)))
                              for the-ordered-units = (sort units-with-boundaries
                                                            #'< :key #'second)
                              collect (list role (mapcar #'first the-ordered-units)))))
    (loop for ordered-unit in ordered-units
          collect (list (first ordered-unit)
                        (if (stringp (second ordered-unit))
                          (second ordered-unit)
                          (format nil "~{~a~^ ~}"
                                  (loop for unit-name in (second ordered-unit)
                                        collect 
                                          (if (stringp unit-name)
                                            unit-name
                                            (fcg::retrieve-string-for-unit unit-name units)))))))))

(defmethod extract-semantic-frames ((utterance string)
                                    (key (eql :dependency-based))
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
           (sem-frames 
            (loop for evoked-frame in evoked-frames
                  for frame-type = (unit-feature-value evoked-frame 'frame-type)
                  for frame-elements = 
                    (loop for feature in (unit-body evoked-frame)
                          unless (eql 'frame-type (feature-name feature))
                            collect feature)
                  when (get-frame-def frame-type)
                    collect (make-instance frame-type
                                           :evoked-by (fcg::retrieve-string-for-unit 
                                                       (unit-name evoked-frame) transient-units)
                                           :frame-elements 
                                           (collect-frame-elements-through-dependency-tree
                                            (unit-name evoked-frame) frame-elements transient-units t)))))
      ;; To be replaced by a real web monitor.
      (when frame-dev-monitor
        (loop for sem-frame in sem-frames
              do (add-element (make-html sem-frame :expand-initially t))))
      sem-frames)))

;; ------------------------------------------------------------------------
;; Procedural Attachment Methods
;; ------------------------------------------------------------------------

(export '(unit-append))

(defmethod frame-extractor-procedure ((procedure (eql 'unit-append))
                                      (body list)
                                      (units list))
  (declare (ignore procedure))
  (loop for unit-name in body
        append (if (listp unit-name)
                 (apply-frame-extractor-procedure unit-name units)
                 (retrieve-lexical-boundaries-for-unit unit-name units))))

;; Deprecated code:
#|
(defun format-units-as-string (unit-names units)
  "String-append the string associated with a unit, separating them by a white space."
  (format nil "~{~a~^ ~}"
          (mapcar #'(lambda(unit-name)
                      (retrieve-string-for-unit unit-name units))
                  (flatten unit-names))))

(defmethod frame-extractor-procedure ((procedure (eql 'unit-remove))
                                      (body list)
                                      (units list))
  (declare (ignore procedure))
  (format-units-as-string (remove (first body) (second body)) units))
|#
