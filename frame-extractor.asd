;; Copyright 2021-present
;;           Sony Computer Science Laboratories Paris
;;           Remi van Trijp (http://www.remivantrijp.eu)

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

(in-package #:asdf)

(defsystem :frame-extractor
  :author "Remi van Trijp <remi.vantrijp@sony.com>"
  :version "1.0"
  :license "Apache 2.0-License"
  :depends-on (:fcg
               :irl
               :xmls
               :nlp-tools
               :fcg-hybrids
               :plot-raw-data
               :category-hierarchies)
  :serial t
  :components ((:file "class-and-macros")
               (:file "semantic-frames")
               (:file "fcg-expand")
               (:file "extract-frames")
               (:file "export-frames")
               (:file "load-models"))
  :description "Library of semantic frame  extractors for hybrid FCG-grammars.")
