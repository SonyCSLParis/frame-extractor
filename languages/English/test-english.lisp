
(in-package :fcg)
; (progn (deactivate-monitor trace-fcg) (test-causation-frame))
; (activate-monitor trace-fcg)

#|
(extract-semantic-frames "At the entrance of the Mansour Hotel, Hall made a brief statement, saying that he came to Iraq because he has heard for a long time about the humanitarian situation here, especially the malnutrition among the Iraqi children and the increasing mortality rate of the children and the elderly"
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "The education act brought about significant changes"
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "The education act has brought about significant changes"
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

(extract-semantic-frames "The children of poorer families are condemned to remain poor all their lives because opportunities are reserved for the children of richer families"
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Environmental businesses in the UK have been hit in recent years by swings in government policy that have led to job losses and uncertainty among potential investors."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "It would be at a level that might allow Australia to meet its targets because of accounting rules."
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
(extract-semantic-frames "The growth is due to the rapidly falling costs of renewables."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "This would result in a sea level rise that would inundate millions of Americans homes, cause punishing heat waves, trigger the spread of disease and disastrous extreme weather events."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

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

(extract-semantic-frames "What she said is funny because it is true"
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "He says you like to swim because you love water"
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

;; Does not work because "about rising sea levels" is parsed as a prepositional object by the dependency parser
;; but correctly by the constituent parser.
(extract-semantic-frames "Global warming has brought about rising sea levels."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

(extract-semantic-frames "Global warming has probably resulted in rising sea levels."
                         :phrase-based
                         :cxn-inventory *fcg-english*
                         :frame-dev-monitor t)

|#