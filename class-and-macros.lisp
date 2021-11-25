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

;;; ----------------------------------------------------------------------------
;;; The *frame-library* will contain definitions that FCG constructions can use.
;;; ----------------------------------------------------------------------------

(export '(get-frame-def get-frame-elements get-frame-evokes get-frame-documentation get-frame-url))

(defvar *frame-library* (make-hash-table) 
  "All definitions of semantic frames will be stored in *frame-library*")

(defstruct frame-def frame-elements url evokes documentation)

;; Helper functions
(defun get-frame-def (name)
  (gethash name *frame-library*))

(defun get-frame-elements (name)
  (frame-def-frame-elements (if (symbolp name) (get-frame-def name) name)))

(defun get-frame-evokes (name)
  (frame-def-evokes (if (symbolp name) (get-frame-def name) name)))

(defun get-frame-documentation (name)
  (frame-def-documentation (if (symbolp name)  (get-frame-def name) name)))

(defun get-frame-url (name)
  (frame-def-url (if (symbolp name)  (get-frame-def name) name)))

;;; ----------------------------------------------------------------------------
;;; Semantic Frame Classes and Subclasses
;;; ----------------------------------------------------------------------------

(export '(semantic-frame def-sem-frame frame-type frame-elements evoked-by evokes))

(defclass semantic-frame (irl::entity)
  ((frame-type :initarg :frame-type :accessor frame-type
               :documentation "Contains the FRAME-TYPE predicate (e.g. CAUSE).")
   (frame-elements :initarg :frame-elements :accessor frame-elements
                   :documentation "Contains the roles of a frame (e.g. CAUSER and EFFECT).")
   (evoked-by :initarg :evoked-by :accessor evoked-by
              :documentation "Contains the string that evoked the semantic frame.")
   (evokes :initarg :evokes :accessor evokes :initform nil
           :documentation "A semantic frame may evoke a more abstract semantic frame."))   
  (:documentation "A base class for storing semantic frames and visualizing them in the web interface."))

;;; ----------------------------------------------------------------------------
;;; Helper Macros
;;; ----------------------------------------------------------------------------

(defmacro def-frame-supporting-html-method (frame-class)

  `(defmethod irl::make-html-for-entity-details ((frame ,frame-class)
                                            &key &allow-other-keys)
     `(((table)
        ((tr)
         ((td) ((b) "Frame Type"))
         ((td) ,(frame-type frame)))
        ((tr)
         ((td) ((b) "Frame Elements"))
         ((td) ,@(loop for element-value in (frame-elements frame)
                       for element = (first element-value)
                       for value =  (second element-value)
                       collect `((tr)
                                 ((td) ((b) ((em) ,(format nil "~@(~a~):" element))))
                                 ((td) ((em) ,value))))))
        ((tr)
         ((td) ((b) "Evoked by"))
         ((td) ,(evoked-by frame)))))))

(defmacro def-sem-frame (name frame-elements &key url evokes documentation)
  `(progn
     ;; Define a class for the semantic frame:
     (defclass ,name (semantic-frame) ())

     ;; We add or replace the definition in our store of frame definitions
     (setf (gethash ',name *frame-library*)
           (make-frame-def :frame-elements ',frame-elements
                           :url ,url
                           :evokes ',evokes
                           :documentation ,documentation))

     ;; Define an initialization method:
     (defmethod initialize-instance :after ((sem-frame ,name)
                                            &key &allow-other-keys)
       (setf (frame-type sem-frame) ',name))
     ;; Define web interface methods:
     (def-frame-supporting-html-method ,name)))