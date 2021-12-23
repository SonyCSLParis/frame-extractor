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

(deftest test-causation-frame ()
  (let ((testcases (list (list "Many linguists, because of their hatred of Chomsky, avoid to formalize their work."
                               "their hatred of Chomsky"
                               "Many linguists avoid to formalize their work")
                         (list "Many linguists avoid formalization because of their hatred of Chomsky."
                               "their hatred of Chomsky"
                               "Many linguists avoid formalization")
                         (list "because of their hatred of Chomsky, many linguists avoid formalization."
                               "their hatred of Chomsky"
                               "many linguists avoid formalization")
                         (list "I came to France because I was offered a job by Sony."
                               "I was offered a job by Sony"
                               "I came to France")
                         (list "because I was offered a job by Sony, I came to Paris"
                               "I was offered a job by Sony"
                               "I came to Paris")
                         (list "The oldest group in the country is dominated by men, a fact that is particularly galling to women because the group is the sole source of ringers for the most prestigious churches."
                               "the group is the sole source of ringers for the most prestigious churches"
                               "a fact that is particularly galling to women")
                         (list "But because these claims are more difficult to evaluate and have been coming in more slowly, the company has no estimate of the impact of the earthquake on its quarter results."
                               "these claims are more difficult to evaluate and have been coming in more slowly"
                               "the company has no estimate of the impact of the earthquake on its quarter results"))))
    (dolist (testcase testcases)
      (let* ((sem-frame (first (extract-semantic-frames (first testcase) :phrase-based :cxn-inventory *fcg-english*)))
             (frame-elements (frame-elements sem-frame)))
        (test-assert (string= (second (assoc 'cause frame-elements)) (second testcase)))
        (test-assert (string= (second (assoc 'effect frame-elements)) (third testcase)))))))

; (progn (deactivate-monitor trace-fcg) (test-causation-frame))
; (activate-monitor trace-fcg)

(extract-semantic-frames "Many linguists, because of their hatred of Chomsky, avoid to formalize their work."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Many linguists avoid formalization because of their hatred of Chomsky."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Many linguists have unfortunately avoided formalization because of their hatred of Chomsky."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "I came to France because I was offered a job by Sony."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "because I was offered a job by Sony, I came to France."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "The oldest group in the country is dominated by men, a fact that is particularly galling to women because the group is the sole source of ringers for the most prestigious churches."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "But because these claims are more difficult to evaluate and have been coming in more slowly, the company has no estimate of the impact of the earthquake on its quarter results."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "According to biblical tradition , although David bought the land for the Temple and carefully assembled its building materials , he was deemed unworthy of constructing the Temple because he was a man of war with blood on his hands"
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "He has been considered to be a king"
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)
