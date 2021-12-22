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

(export '(export-semantic-frames))

(defun export-semantic-frames (semantic-frames filename &key (if-exists :supersede))
  "Write the semantic frames to a file."
  (with-open-file (out filename :direction :output
                       :if-exists if-exists)
    (cl-json:encode-json semantic-frames out)))
; (export-semantic-frames (list (make-instance 'causation)) (babel-pathname :name "test" :type "json"))

;; Todo: Functions for server (responding to http requests)