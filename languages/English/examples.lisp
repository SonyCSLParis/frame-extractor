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

(extract-semantic-frames "Many linguists, because of their hatred of Chomsky, avoid to formalize their work."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Many linguists, due to their hatred of Chomsky, avoid to formalize their work."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Many linguists have unfortunately avoided formalization because of their hatred of Chomsky."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Many linguists have unfortunately avoided formalization because they hate Chomsky."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Many linguists have unfortunately avoided formalization due to their hatred of Chomsky."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "I came to France because I was offered a job by Sony."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "I have come to France because I was offered a job by Sony."
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

(extract-semantic-frames "According to biblical tradition, although David bought the land for the Temple and carefully assembled its building materials, he was deemed unworthy of constructing the Temple because he was a man of war with blood on his hands"
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)


(activate-monitor trace-fcg)
(comprehend "According to biblical tradition, although David bought the land for the Temple and carefully assembled its building materials, he was deemed unworthy of constructing the Temple because he was a man of war with blood on his hands" :cxn-inventory *test-grammar*)
