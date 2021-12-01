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


(defgeneric extract-semantic-frames (utterance key &key cxn-inventory key  &allow-other-keys))

;; Method that only cares about the strings. This relies on the use of BOUNDARIES.
;; ----------------------------------------------------------------------------------------------------------------------------

(defun lexical-boundary-p (boundary)
  (= 1 (- (third boundary) (second boundary))))

(defun retrieve-string-for-unit (unit-name units)
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
    (format nil "~{~a~^ ~}" strings)))

(defmethod extract-semantic-frames ((utterance string)
                                    (key (eql :sem-frame))
                                    &key cxn-inventory
                                    frame-dev-monitor &allow-other-keys)
  (declare (ignore key))
  (multiple-value-bind (meaning final-node)
      (comprehend utterance :cxn-inventory (or cxn-inventory *fcg-constructions*))
    (declare (ignore meaning))
    (let* ((transient-units (fcg-get-transient-unit-structure final-node))
           (evoked-frames (loop for unit in transient-units
                                for sem-frame = (unit-feature-value unit 'sem-frame)
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
;; (extract-semantic-frames "The wind caused damage." :sem-frame :cxn-inventory *fcg-english* :frame-dev-monitor t)