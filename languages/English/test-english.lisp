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
                               "the company has no estimate of the impact of the earthquake on its quarter results")
                         (list "According to biblical tradition , although David bought the land for the Temple and carefully assembled its building materials , he was deemed unworthy of constructing the Temple because he was a man of war with blood on his hands"
                               "he was a man of war with blood on his hands"
                               "he was deemed unworthy of constructing the Temple")
                         (list "The education act brought about significant changes"
                               "The education act"
                               "significant changes")
                         (list "Fanatical warriors inspired by a new and militaristic religion, Islam, brought about the final break between East and West ."
                               "Fanatical warriors inspired by a new and militaristic religion , Islam ,"
                               "the final break between East and West")
                         (list "These scientists are experimenting with the causative agents of such diseases as smallpox and the plague."
                               "agents"
                               "such diseases as smallpox and the plague")
                         (list "The Republic was brought about by a heroic revolution"
                               "a heroic revolution"
                               "The Republic")
                         (list "The perpetual hustle has led to unpredictability."
                               "The perpetual hustle"
                               "unpredictability")
                         (list "Environmental businesses in the UK have been hit in recent years by swings in government policy that have led to job losses and uncertainty among potential investors."
                               "swings in government policy"
                               "job losses and uncertainty among potential investors"))))
    (dolist (testcase testcases)
      (let* ((sem-frame (first (extract-semantic-frames (first testcase) :phrase-based :cxn-inventory *fcg-english*)))
             (frame-elements (frame-elements sem-frame)))
        (test-assert (string= (second (assoc 'cause frame-elements)) (second testcase)))
        (test-assert (string= (second (assoc 'effect frame-elements)) (third testcase)))))))

; (progn (deactivate-monitor trace-fcg) (test-causation-frame))
; (activate-monitor trace-fcg)




(extract-semantic-frames "At the entrance of the Mansour Hotel, Hall made a brief statement, saying that he came to Iraq because he has heard for a long time about the humanitarian situation here, especially the malnutrition among the Iraqi children and the increasing mortality rate of the children and the elderly"
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "The education act brought about significant changes"
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Fanatical warriors inspired by a new and militaristic religion, Islam, brought about the final break between East and West ."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "These scientists are experimenting with the causative agents of such diseases as smallpox and the plague."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "The perpetual hustle has led to unpredictability."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "But soaring temperatures in the Arctic at the end of the world's hottest ever recorded year led to melting and heavy rain, when light snow should have been falling."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Environmental businesses in the UK have been hit in recent years by swings in government policy that have led to job losses and uncertainty among potential investors."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "This would result in a sea level rise that would inundate millions of Americans homes, cause punishing heat waves, trigger the spread of disease and disastrous extreme weather events."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Global warming has probably resulted in rising sea levels."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "It would be at a level that might allow Australia to meet its targets because of accounting rules."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "The growth is due to the rapidly falling costs of renewables."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "These scientists are experimenting with the causative agents of such diseases as smallpox and the plague."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "In practice this ordinance brought about little immediate change because it was introduced piecemeal"
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Global warming causes natural disasters to become worse."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Global warming causes a rise in natural disasters."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Research shows that displaced human activity, caused by converting biofuels production, can result in substantial CO2 emmissions."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)


;; To solve:
;; -------------------------------------------------------------------------------------------------------
(extract-semantic-frames "Current climate policies are projected to result in increased greenhouse gas emmissions."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "This will probably result in much more of an emphasis on countering disinformation and lead to much more intelligence gathering in collaboration with Europeans."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "This in turn could lead to multiple instances of conflicts of interest, Swackhammer said, despite clear EPA ethics rules."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "He highlighted the increasing amount that has led to what she branded breakthroughs."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "It's easy enough to think of similar examples and certainly at least some groups would be doomed from such an event simply because of where they were."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)


(extract-semantic-frames "Such a transfer could and would only be brought about by a bourgeois revolution."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Laws to bring this about were moving through parliament when the vote was held."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "No other theme was more suited to bring this about"
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)


(extract-semantic-frames "it was encouraged by Israel because they thought it was a counter weight to Palestinian nationalism which is , of course , a secular movement."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)
