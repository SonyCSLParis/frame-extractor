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

(defmethod fcg-expand ((type (eql :sem-frame))
                       &key value source bindings merge? &allow-other-keys)
  (declare (ignore type source bindings merge?))
  (let* ((frame-type (if (eql '== (first value)) (first (second value)) (first value)))
         (frame-elements (get-frame-elements frame-type))
         (frame-fillers (if (eql '== (first value)) (rest (second value)) (rest value)))
         (features `((frame-type ,frame-type)
                     ,@(cond
                        ;; 1/ The user has defined some frame elements manually:
                        ((and frame-fillers (listp (first frame-fillers)))
                         (append frame-fillers
                                 (loop for fe in  frame-elements
                                       unless (find fe frame-fillers :key #'first)
                                       collect (list fe (make-var 'filler)))))
                        ;; 2/ The user wants variable equalities:
                        ((= (length frame-elements)  (length frame-fillers))
                         (mapcar #'list frame-elements frame-fillers))
                        ;; 3/ The user wants automatic expansion or only specifies variable equalities partially:
                        (t
                         (loop for fe in frame-elements
                               for i from 0 to (1- (length frame-elements))
                               for filler = (nth i frame-fillers)
                               collect (if filler (list fe filler) (list fe (make-var 'filler)))))))))
    (cons '==1 features)))

(export '(activate-frame-extractor sem-frame def-frame-cxn get-frame-feature))

(defun activate-frame-extractor (&key (cxn-inventory *fcg-constructions*)
                                      (frame-feature 'sem-frame))
  "Helper function to ensure that a construction inventory is compatible with the frame extractor."
  (set-configuration cxn-inventory :frame-feature frame-feature)
  (let ((new-type-def (list frame-feature 'set-of-features :sem-frame)) ;'sequence :sem-frame))
        (old-type-def (assoc frame-feature (feature-types cxn-inventory))))
    (if old-type-def
      (setf (feature-types cxn-inventory)
            (substitute new-type-def old-type-def (feature-types cxn-inventory) :test #'equal))
      (push new-type-def (feature-types cxn-inventory)))))

(defun get-frame-feature (cxn-inventory)
  (or (get-configuration cxn-inventory :frame-feature) 'sem-frame))

;; Helper macro
(defmacro def-frame-cxn (cxn-name fs &key (cxn-inventory '*fcg-constructions*)
                                   (cxn-set 'cxn) (score 0.5)
                                   (feature-types nil) (attributes nil)
                                   (disable-automatic-footprints nil) 
                                   (description nil))
  `(def-fcg-cxn ,cxn-name ,fs
                :cxn-inventory ,cxn-inventory
                :cxn-set ,cxn-set
                :score ,score
                :feature-types ,(cons (list 'sem-frame 'sequence :sem-frame) feature-types)
                :attributes ,attributes
                :disable-automatic-footprints ,disable-automatic-footprints
                :description ,description))