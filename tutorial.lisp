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

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; TUTORIAL 1: PHRASE-BASED SEMANTIC FRAMES
;; ----------------------------------------------------------------------------
;; This file shows how to use semantic frames in constructions.
;; We will illustrate the approach using the base model for English from 
;; fcg-hybrids.
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; (ql:quickload :frame-extractor)

(in-package :fcg)

;; We activate the monitor trace-fcg so you can check what is going on 
;; in the web interface at http://localhost:8000/
(activate-monitor trace-fcg)

;; We will test the frame extracor using the base model for English. You can
;; always reset it as follows:
(setf *fcg-english* (make-english-base-model-cxns))
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

;; In order to display both the construction and the transient structure 
;; correctly, some behind-the-scenes feature-type definitions need to be set.
;; This is why we recommend to use the macro def-frame-cxn that works in 
;; exactly the same way as def-fcg-cxn, but which will take care of these 
;; feature-type definitions for  you.

;; 2.a) Automatic Frame Expansion
;;; ----------------------------------------------------------------------------
;; If you introduce a semantic frame for the first time, e.g. as part of 
;; a lexical construction, you can often rely on the automatic expansion
;; of the frame. This means that you only have to mention the frame-type
;; and FCG will automatically expand that into an appropriate feature-set:

; (sem-frame (causation))
(def-frame-cxn caused-lex
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
(def-frame-cxn caused-lex
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
(def-frame-cxn caused-lex
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
(def-frame-cxn caused-lex
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

;; We first introduce the frame using automatic expansion:
(def-frame-cxn caused-lex
               ((?caused-unit
                 (sem-frame (causation)))
                <-
                (?caused-unit
                 --
                 (HASH form ((string ?caused-unit "caused")))))
               :cxn-inventory *fcg-english*)

;; Now we use variable equalities for linking the frame:
(def-frame-cxn 
 active-transitive-causation-frame
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

;; Helper function to test extracting the semantic frame:
; (extract-semantic-frames "The wind caused damage." :phrase-based :cxn-inventory *fcg-english* :frame-dev-monitor t)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; TUTORIAL 2: MEANING-BASED SEMANTIC FRAMES
;; ----------------------------------------------------------------------------
;; This file shows how to use semantic frames as part of the menaing of
;; an utterance. We will illustrate the approach using the base model for English 
;; from fcg-hybrids.
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; 1/ Resetting the base model:
(setf *fcg-english* (make-english-base-model-cxns))

;; 2/ No additional modifications to the transient structure are needed, so 
;;    we can immediately start implementing our grammar.

(def-fcg-cxn caused-lex
             ((?caused-unit
               (referent ?ev)
               (arg-struct (actor ?x)
                           (undergoer ?y)))
              <-
              (?caused-unit
               (HASH meaning ((causation ?ev)
                              (cause ?ev ?x)
                              (effect ?ev ?y)))
               -- 
               (HASH form ((string ?caused-unit "caused")))))
             :cxn-inventory *fcg-english*)

(def-fcg-cxn active-transitive-cxn
             (<-
              (?root-verb
               (arg-struct (actor ?x)
                           (undergoer ?y))
               --
               (functional-structure (subject ?subject)
                                     (direct-object ?object)))
              (?subject
               (referent ?x)
               --
               (parent ?subject-NP))
              (?object
               (referent ?y)
               --
               (parent ?object-NP)))
             :cxn-inventory *fcg-english*)
;; (comprehend "The wind caused damage" :cxn-inventory *fcg-english*)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; TUTORIAL 3: DEPENDENCY-BASED SEMANTIC FRAMES
;; ----------------------------------------------------------------------------
;; This file shows how to extract semantic frames as phrases but using a 
;; dependency parse as the basis. We will illustrate the approach using the 
;; base model for French from fcg-hybrids.
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; We activate the frame extractor code for *fcg-french*:
(activate-frame-extractor :cxn-inventory *fcg-french*)

(def-fcg-cxn provoque-lex
             ((?provoque-unit
               (sem-frame (causation)))
              <-
              (?provoque-unit
               -- 
               (HASH form ((string ?provoque-unit "provoque")))))
             :cxn-inventory *fcg-french*)

(def-frame-cxn active-transitive-causation-frame
 (<-
  (?root-verb
   --
   (functional-structure (subject ?subject)
                         (direct-object ?object))
   (sem-frame (causation ?subject ?object))))
 :cxn-inventory *fcg-french*)

;; Testing:
(extract-semantic-frames "Le changement climatique provoque des orages plus fortes." 
                         :dependency-based
                         :cxn-inventory *fcg-french*
                         :frame-dev-monitor t)
