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

;;; ----------------------------------------------------------------------------
;;; The *frame-library* will contain definitions that FCG constructions can use.
;;; ----------------------------------------------------------------------------

(export '(get-frame-def get-frame-elements get-frame-evokes get-frame-documentation get-frame-url))

(defvar *frame-library* (make-hash-table) 
  "All definitions of semantic frames will be stored in *frame-library*")

(defstruct frame-def frame-elements url evokes documentation)

;; Helper functions
(defun get-frame-def (name)
  "Retrieve a semantic frame definition."
  (gethash name *frame-library*))

(defun get-frame-elements (name)
  "Retrieve frame elements of a frame definition."
  (let ((definition (if (symbolp name)
                      (get-frame-def name)
                      name)))
    (when definition
      (frame-def-frame-elements definition))))

(defun get-frame-evokes (name)
  "Retrieve which frames are evoked by a frame."
  (let ((definition (if (symbolp name)
                      (get-frame-def name)
                      name)))
    (frame-def-evokes definition)))

(defun get-frame-documentation (name)
  "Retrieve the documentation of a semantic frame."
  (let ((definition (if (symbolp name)
                      (get-frame-def name)
                      name)))
    (frame-def-documentation definition)))

(defun get-frame-url (name)
  "Retrieve the URL associated with a semantic frame."
  (let ((definition (if (symbolp name)
                      (get-frame-def name)
                      name)))
    (frame-def-url definition)))

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