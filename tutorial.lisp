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

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; This file shows how to use semantic frames in constructions.
;; We will illustrate the approach using the base model for English from 
;; fcg-hybrids.
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; (ql:quickload :frame-extractor)

(in-package :fcg)

;; We activate the monitor trace-fcg so you can check what is going on 
;; in the web interface at http://localhost:8000/
(activate-monitor trace-fcg)

;; We will test the frame extracor using the base model for English
;; from FCG hybrids on the following simple sentence:
(comprehend "The wind caused damage." :cxn-inventory *fcg-english*)

;; 1. How to configure your construction inventory
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; We can make a construction inventory "frame extractor"-ready as follows:
(activate-frame-extractor :cxn-inventory *fcg-english*)

;; By default, this function assumes that the grammar will have a feature 
;; called "sem-frame". You can customize this using the keyword :frame-feature
(activate-frame-extractor :cxn-inventory *fcg-english*
                          :frame-feature 'sem-frame)

;; For those who insist on manually setting the construction inventory,
;; please check the function #'activate-frame-extractor in fcg-expand.lisp.

;; 2.Using semantic frames in constructions 
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; 2.a) Automatic Frame Expansion
;;; ----------------------------------------------------------------------------
;; If you introduce a semantic frame for the first time, e.g. as part of 
;; a lexical construction, you can often rely on the automatic expansion
;; of the frame. This means that you only have to mention the frame-type
;; and FCG will automatically expand that into an appropriate feature-set:

; (sem-frame (causation))
(def-fcg-cxn caused-lex
             ((?caused-unit
               (sem-frame (causation)))
              <-
               (?caused-unit
                --
                (HASH form ((string ?caused-unit "caused")))))
             :cxn-inventory *fcg-english*)
;; Compare what you see in the construction with the result in the transient structure:
;; (comprehend "The wind caused damage." :cxn-inventory *fcg-english*)

;; 2.b) Automatic Frame Expansion with variable Equalities
;;; ----------------------------------------------------------------------------
;; In the previous example, I assumed that I later want to link the semantic frame
;; to units in the transient structure, but some researchers might be  more interested
;; in e.g. linking the frame to the MEANING of an utterance. In both cases, we 
;; want to  be able to use VARIABLE EQUALITIES for doing so. You can automatically 
;; expand a semantic frame and assign variable equalities as follows:

; (frame-type ?var-1 ?var-2 ... ?var-n)

;; Important is that you respect the order of the frame elements as defined 
;; in the semantic frame library. You can always check the order and names 
;; of frame elements as follows:

; (get-frame-elements 'causation)

;; In this example we will link the frame elements to the meaning of the utterance:
(def-fcg-cxn caused-lex
             ((?caused-unit
               (sem-frame (causation ?x ?y)))
              <-
               (?caused-unit
                (HASH meaning ((cause ?ev ?x ?y)))
                --
                (HASH form ((string ?caused-unit "caused")))))
             :cxn-inventory *fcg-english*)
;; Compare what you see in the construction with the result in the transient structure:
;; (comprehend "The wind caused damage." :cxn-inventory *fcg-english*)

;; If you are only interested in linking the first frame element(s), you do not
;; need an explicit variable for the remaining ones. However, you cannot "skip" 
;; a variable. For example, (causation ?y) cannot be used for linking ?y to the 
;; EFFECT of the causation frame because EFFECT is the second frame element. Indeed,
;; ?y would be linked to the CAUSE of the causation frame:
(def-fcg-cxn caused-lex
             ((?caused-unit
               (sem-frame (causation ?y)))
              <-
               (?caused-unit
                (HASH meaning ((cause ?ev ?x ?y)))
                --
                (HASH form ((string ?caused-unit "caused")))))
             :cxn-inventory *fcg-english*)
;; Compare what you see in the construction with the result in the transient structure:
;; (comprehend "The wind caused damage." :cxn-inventory *fcg-english*)


;; 2.c) Defining the frame manually (or partially manually)
;;; ----------------------------------------------------------------------------
;; The above method of variable equalities requires you to respect the order 
;; of the frame elements of a semantic frame. If you want to specify only one 
;; or more frame elements without worrying about their order, you can mention 
;; the name of the frame element explictly as follows:

; (frame-name
;  (frame-element-1 ?var-1)
;  (frame-element-2 ?var-2)
;   ...
;  (frame-element-n ?var-n))

;; Here, you have to be carful thought to use the CORRECT names of the 
;; frame elements.
;; Here is an example:
(def-fcg-cxn caused-lex
             ((?caused-unit
               (sem-frame (causation
                           (effect ?y))))
              <-
               (?caused-unit
                (HASH meaning ((cause ?ev ?x ?y)))
                --
                (HASH form ((string ?caused-unit "caused")))))
             :cxn-inventory *fcg-english*)
; (comprehend "The wind caused damage." :cxn-inventory *fcg-english*)


;; 3. An example
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Here is an example of how to use the frame extractor in which the fillers 
;; of a slot are bound to phrasal units. The goal of such an extractor is to 
;; identify sequences in the utterance that fill the frame element slots of 
;; the semantic frame.
(def-fcg-cxn caused-lex
             ((?caused-unit
               (sem-frame (causation)))
              <-
               (?caused-unit
                --
                (HASH form ((string ?caused-unit "caused")))))
             :cxn-inventory *fcg-english*)

(def-fcg-cxn active-transitive-causation-frame
             (<-
              (?root-verb
               --
               (functional-structure (subject ?subject)
                                     (direct-object ?object))
               (sem-frame (causation ?subject-NP ?object-NP)))
              (?subject
               --
               (parent ?subject-NP))
              (?object
               --
               (parent ?object-NP)))
             :cxn-inventory *fcg-english*)
; (comprehend "The wind caused damage." :cxn-inventory *fcg-english*)

