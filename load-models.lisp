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

(defparameter *frame-extractor-pathname*
  (make-pathname :directory (pathname-directory (or *load-truename*
						    *compile-file-truename*))))

(dolist (pathname '("frame-extractor-models/Italian/ife.asd")) ; Italian
  (load (merge-pathnames pathname *frame-extractor-pathname*)))

(defun load-base-model (language &key (perform-tests nil))
  "Load the base model for a particular language."
  (multiple-value-bind (the-language cxn-inventory)
      (case language
        (:english
         (setf *fcg-english* (make-english-base-model-cxns))
         (activate-frame-extractor :cxn-inventory *fcg-english*)
         (values "English" *fcg-english*))
        (t
         (error (format nil "There is no language model for keyword ~a" language))))
    (loop for file in  (directory (merge-pathnames (make-pathname :directory `(:relative "languages" ,the-language)
                                                                  :name "*"
                                                                  :type "lisp")
                                                   *frame-extractor-pathname*))
          do (load file))
    (when perform-tests
      (load (merge-pathnames (make-pathname :directory '(:relative "tests")
                                            :name the-language
                                            :type "lisp")
                             *frame-extractor-pathname*)))
    cxn-inventory))
          