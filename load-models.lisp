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

(defparameter *frame-extractor-pathname*
  (make-pathname :directory (pathname-directory (or *load-truename*
						    *compile-file-truename*))))

(dolist (pathname '("frame-extractor-models/Italian/ife.asd" ; Italian
                    "frame-extractor-models/English/efe.asd")) ; English
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