;;; Copyright (C) Sony Computer Science Laboratories Paris
;;;               Author: Remi van Trijp (www.remivantrijp.eu)
;;; 
;;;     This program is free software: you can redistribute it and/or modify
;;;     it under the terms of the GNU General Public License as published by
;;;     the Free Software Foundation, version 3 of the License.
;;; 
;;;     This program is distributed in the hope that it will be useful,
;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;     GNU General Public License for more details.
;;; 
;;;     You should have received a copy of the GNU General Public License
;;;     along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------------

(in-package :fcg)

(activate-monitor trace-fcg)

;; We will test a simple sentence using the base model for English from fcg-hybrids.
(comprehend "The wind caused damage." :cxn-inventory *fcg-english*)

(get-frame-elements 'causation)

(defmethod fcg-expand ((type (eql :sem-frame))
                       &key value source bindings merge? &allow-other-keys)
  (declare (ignore type source bindings merge?))
  (let* ((frame-type (first value))
         (frame-elements (get-frame-elements frame-type))
         (frame-fillers (rest value))
         (features `((frame-type ,frame-type)
                     ,@(if (= (length frame-elements) (length frame-fillers))
                         (mapcar #'list frame-elements frame-fillers)
                         (loop for fe in frame-elements
                               collect (list fe (make-var 'filler)))))))
    (cons '==1 features)))

(def-fcg-cxn caused-lex
             ((?caused
               (sem-frame (causation ?cause ?effect)))
              <-
              (?caused
               (HASH meaning ((cause ?ev ?cause ?effect)))
               --
               (HASH form ((string ?caused "caused")))))
             :feature-types ((sem-frame sequence :sem-frame))
             :cxn-inventory *fcg-english*)
; set-of-feature-value-pairs