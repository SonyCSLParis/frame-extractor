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

;; Causation frames
;; ================
;; BECAUSE: introduces an "CAUSE phrase"
(def-frame-cxn because-lex
               (<-
                (?because
                 (sem-frame (causation
                             (cause ?cause-phrase)))
                 (lex-id because)
                 --
                 (hash form ((string ?because "because")))
                 (parent ?because-phrase))
                (?because-phrase
                 (sem-cat (phrase-type cause-phrase))
                 --
                 (hash form ((meets ?because ?cause-phrase ?because-phrase)))
                 (constituents (?because ?cause-phrase)))
                (?cause-phrase
                 --
                 (syn-cat (not (lex-class ?lex-class)))))
               :attributes (:label :hashed-string
                            :string "because")
               :cxn-inventory *fcg-english*)

;; BECAUSE OF
(def-frame-cxn because-of-lex
               (<-
                (?because
                 (sem-frame (causation
                             (cause ?cause-unit)))
                 (lex-id because)
                 --
                 (hash form ((string ?because "because")
                             (string ?of "of")))
                 (parent ?because-phrase))
                (?because-phrase
                 (sem-cat (phrase-type cause-phrase))
                 --
                 (hash form ((meets ?because ?of ?because-phrase)
                             (meets ?of ?cause-unit ?because-phrase)))
                 (constituents (?because ?of ?cause-unit))))
               :attributes (:label :hashed-string
                            :string "because")
               :cxn-inventory *fcg-english*)


;; BRING ABOUT
(def-fcg-cxn BRING->brings-morph
             ((?brings-unit
               (footprints (number morph)))
              <-
              (?brings-unit
               (lex-id bring)
               (footprints (not number morph))
               (syn-cat (verb-form base-form)
                        (lex-class verb)
                        (finite +)
                        (agreement (- - + -)))
               --
               (HASH form ((string ?brings-unit "brings")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "brings")
            :cxn-inventory *fcg-english*)

(def-fcg-cxn BRING->bringing-morph
             ((?bringing-unit
               (footprints (number morph)))
              <-
              (?bringing-unit
               (footprints (not number morph))
               (lex-id bring)
               (syn-cat (verb-form ing-form)
                        (lex-class verb)
                        (finite -)
                        (agreement ?agr))
               --
               (HASH form ((string ?bringing-unit "bringing")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "bringing")
            :cxn-inventory *fcg-english*)

(def-fcg-cxn BRING->bring-morph
             ((?bring-unit
               (footprints (number morph)))
              <-
              (?bring-unit
               (lex-id bring)
               (footprints (not number morph))
               (syn-cat (verb-form base-form)
                        (lex-class verb)
                        (finite ?finite)
                        (agreement ?agr))
               --
               (HASH form ((string ?bring-unit "bring")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "bring")
            :cxn-inventory *fcg-english*)

(def-fcg-cxn BRING->brought-morph
             ((?brought-unit
               (footprints (number morph)))
              <-
              (?brought-unit
               (lex-id bring)
               (footprints (not number morph))
               (syn-cat (verb-form ed-form)
                        (lex-class verb)
                        (finite -)
                        (agreement ?agr))
               --
               (HASH form ((string ?brought-unit "brought")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "brought")
            :cxn-inventory *fcg-english*)

(def-frame-cxn bring-about-lex
               (<-
                (?bring
                 (sem-frame (causation))
                 --
                 (parent ?vp)
                 (lex-id bring)
                 (hash form ((string ?about "about")
                             (meets ?bring ?about ?vp)))))
               :attributes (:label hashed-lex-id :lex-id bring)
               :cxn-inventory *fcg-english*)

;; LEAD TO
(def-fcg-cxn LEAD->leads-morph
             ((?leads-unit
               (footprints (number morph)))
              <-
              (?leads-unit
               (lex-id lead)
               (footprints (not number morph))
               (syn-cat (verb-form base-form)
                        (lex-class verb)
                        (finite +)
                        (agreement (- - + -)))
               --
               (HASH form ((string ?leads-unit "leads")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "leads")
            :cxn-inventory *fcg-english*)

(def-fcg-cxn LEAD->leading-morph
             ((?leading-unit
               (footprints (number morph)))
              <-
              (?leading-unit
               (footprints (not number morph))
               (lex-id lead)
               (syn-cat (verb-form ing-form)
                        (lex-class verb)
                        (finite -)
                        (agreement ?agr))
               --
               (HASH form ((string ?leading-unit "leading")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "leading")
            :cxn-inventory *fcg-english*)

(def-fcg-cxn LEAD->lead-morph
             ((?lead-unit
               (footprints (number morph)))
              <-
              (?lead-unit
               (lex-id lead)
               (footprints (not number morph))
               (syn-cat (verb-form base-form)
                        (lex-class verb)
                        (finite ?finite)
                        (agreement ?agr))
               --
               (HASH form ((string ?lead-unit "lead")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "lead")
            :cxn-inventory *fcg-english*)

(def-fcg-cxn LEAD->led-morph
             ((?led-unit
               (footprints (number morph)))
              <-
              (?led-unit
               (lex-id lead)
               (footprints (not number morph))
               (syn-cat (verb-form ed-form)
                        (lex-class verb)
                        (finite ?finite)
                        (agreement ?agr))
               --
               (HASH form ((string ?led-unit "led")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "led")
            :cxn-inventory *fcg-english*)

(def-frame-cxn led-to-lex
               (<-
                (?lead
                 (sem-frame (causation))
                 --
                 (parent ?vp)
                 (lex-id lead)
                 (hash form ((string ?to "to")
                             (meets ?lead ?to ?vp)))))
               :attributes (:label hashed-lex-id :lex-id lead)
               :cxn-inventory *fcg-english*)

;; RESULT IN
(def-fcg-cxn RESULT->results-morph
             ((?results-unit
               (footprints (number morph)))
              <-
              (?results-unit
               (lex-id result)
               (footprints (not number morph))
               (syn-cat (verb-form base-form)
                        (lex-class verb)
                        (finite +)
                        (agreement (- - + -)))
               --
               (HASH form ((string ?results-unit "results")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "results")
            :cxn-inventory *fcg-english*)

(def-fcg-cxn RESULT->resulting-morph
             ((?resulting-unit
               (footprints (number morph)))
              <-
              (?resulting-unit
               (footprints (not number morph))
               (lex-id result)
               (syn-cat (verb-form ing-form)
                        (lex-class verb)
                        (finite -)
                        (agreement ?agr))
               --
               (HASH form ((string ?resulting-unit "resulting")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "resulting")
            :cxn-inventory *fcg-english*)

(def-fcg-cxn RESULT->result-morph
             ((?result-unit
               (footprints (number morph)))
              <-
              (?result-unit
               (lex-id result)
               (footprints (not number morph))
               (syn-cat (verb-form base-form)
                        (lex-class verb)
                        (finite ?finite)
                        (agreement ?agr))
               --
               (HASH form ((string ?result-unit "result")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "result")
            :cxn-inventory *fcg-english*)

(def-fcg-cxn RESULT->resulted-morph
             ((?resulted-unit
               (footprints (number morph)))
              <-
              (?resulted-unit
               (lex-id result)
               (footprints (not number morph))
               (syn-cat (verb-form ed-form)
                        (lex-class verb)
                        (finite ?finite)
                        (agreement ?agr))
               --
               (HASH form ((string ?resulted-unit "resulted")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "resulted")
            :cxn-inventory *fcg-english*)

(def-frame-cxn result-in-lex
               (<-
                (?result
                 (sem-frame (causation
                             (effect ?some-parent)))
                 (functional-structure
                  (direct-object ?object))
                 --
                 (parent ?vp)
                 (lex-id result)
                 (hash form ((string ?in "in")
                             (meets ?result ?in ?vp))))
                (?in
                 --
                 (parent ?some-parent))
                (?object
                 --
                 (dep-head ?in))
                (?some-parent
                 --
                 (syn-cat (phrase-type ?phrase-type))
                 (constituents (?in ?effect))
                 (hash form ((meets ?in ?effect ?some-parent)))))
               :attributes (:label hashed-lex-id :lex-id result)
               :cxn-inventory *fcg-english*)

;; CAUSE
(def-fcg-cxn CAUSE->causes-morph
             ((?causes-unit
               (footprints (number morph)))
              <-
              (?causes-unit
               (lex-id cause)
               (footprints (not number morph))
               (syn-cat (verb-form base-form)
                        (lex-class verb)
                        (finite +)
                        (agreement (- - + -)))
               --
               (HASH form ((string ?causes-unit "causes")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "causes")
            :cxn-inventory *fcg-english*)

(def-fcg-cxn CAUSE->causing-morph
             ((?causing-unit
               (footprints (number morph)))
              <-
              (?causing-unit
               (footprints (not number morph))
               (lex-id cause)
               (syn-cat (verb-form ing-form)
                        (lex-class verb)
                        (finite -)
                        (agreement ?agr))
               --
               (HASH form ((string ?causing-unit "causing")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "causing")
            :cxn-inventory *fcg-english*)

(def-fcg-cxn CAUSE->cause-morph
             ((?cause-unit
               (footprints (number morph)))
              <-
              (?cause-unit
               (lex-id cause)
               (footprints (not number morph))
               (syn-cat (verb-form base-form)
                        (lex-class verb)
                        (finite ?finite)
                        (agreement ?agr))
               --
               (HASH form ((string ?cause-unit "cause")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "cause")
            :cxn-inventory *fcg-english*)

(def-fcg-cxn CAUSE->caused-morph
             ((?caused-unit
               (footprints (number morph)))
              <-
              (?caused-unit
               (lex-id cause)
               (footprints (not number morph))
               (syn-cat (verb-form ed-form)
                        (lex-class verb)
                        (finite ?finite)
                        (agreement ?agr))
               --
               (HASH form ((string ?caused-unit "caused")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string :string "caused")
            :cxn-inventory *fcg-english*)

(def-frame-cxn cause-lex
               (<-
                (?cause
                 (sem-frame (causation))
                 --
                 (lex-id cause)))
               :attributes (:label hashed-lex-id :lex-id cause)
               :cxn-inventory *fcg-english*)


;; DUE TO
(def-frame-cxn due-to-lex
               ((?cause-phrase
                 (sem-cat (phrase-type cause-phrase)))
                <-
                (?due
                 (sem-frame (causation
                             (cause ?cause-unit)))
                 (lex-id due-to)
                 --
                 (hash form ((string ?due "due")
                             (string ?to "to")
                             (meets ?due ?to ?cause-phrase)))
                 (parent ?cause-phrase)) ;; is sometimes equal to ?some-phrase, sometimes not
                (?to
                 --
                 (parent ?some-phrase))
                (?some-phrase
                 --
                 (constituents (?to ?cause-unit))
                 (hash form ((meets ?to ?cause-unit ?some-phrase)))))
               :attributes (:label hashed-string :string "due")
               :cxn-inventory *fcg-english*)

;; CAUSATIVE
(def-frame-cxn causative-X-of-Y-lex
               (<-
                (?causative
                 (sem-frame (causation
                             (cause ?head)
                             (effect ?effect-phrase)))
                 --
                 (hash form ((string ?causative "causative")
                             (string ?of "of")))
                 (dep-head ?head)
                 (parent ?np))
                (?np
                 --
                 (syn-cat (phrase-type noun-phrase))
                 (constituents (?causative ?head ?pp))
                 (hash form ((meets ?head ?pp ?np))))
                (?pp
                 --
                 (syn-cat (phrase-type prepositional-phrase))
                 (constituents (?of ?effect-phrase))
                 (hash form ((meets ?of ?effect-phrase ?pp)))))
               :attributes (:label hashed-string :string "causative")
               :cxn-inventory *fcg-english*)