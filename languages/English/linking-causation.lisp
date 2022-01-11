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

(def-frame-cxn subject-vp-cause-phrase-cxn
               (<-
                (?sentence
                 --
                 (syn-cat (clause-type ?clause-type))
                 (constituents (?subject-phrase ?cause-phrase ?verb-phrase)))
                (?cause-phrase
                 --
                 (parent ?sentence)
                 (sem-cat (phrase-type cause-phrase))
                 (constituents (?frame-evoking-unit)))
                (?frame-evoking-unit
                 --
                 (sem-frame (causation
                             (effect (unit-append ?subject-phrase ?verb-phrase)))))
                (?verb-phrase
                 --
                 (syn-cat (phrase-type verb-phrase))
                 (parent ?sentence))
                (?subject-phrase
                 --
                 (parent ?sentence)
                 (syn-cat (phrase-type noun-phrase))))
               :cxn-inventory *fcg-english*)

(def-frame-cxn subject-vp-incl-cause-phrase-cxn
               (<-
                (?sentence
                 --
                 (syn-cat (clause-type ?clause-type))
                 (constituents (?subject-phrase ?verb-phrase)))
                (root
                 --
                 (form ((meets ?subject-phrase ?verb-phrase ?sentence))))
                (?verb-phrase
                 --
                 (syn-cat (phrase-type verb-phrase))
                 (constituents ?vp-constituents))
                (?cause-phrase
                 --
                 (parent ?verb-phrase)
                 (sem-cat (phrase-type cause-phrase))
                 (constituents (?frame-evoking-unit)))
                (?frame-evoking-unit
                 --
                 (sem-frame (causation
                             (effect (unit-append ?subject-phrase (unit-remove ?cause-phrase ?vp-constituents)))))))
               :cxn-inventory *fcg-english*)

(def-frame-cxn subject-that-VP-incl-because-phrase-cxn
               (<-
                (?top-noun-phrase
                 --
                 (syn-cat (phrase-type noun-phrase))
                 (constituents ?top-constituents))
                (?that-clause
                 --
                 (parent ?top-noun-phrase)
                 (syn-cat (clause-type subordinate))
                 (constituents (?wh-phrase ?subclause))
                 (hash form ((meets ?wh-phrase ?subclause ?that-clause))))
                (?subclause
                 --
                 (syn-cat (clause-type ?clause-type))
                 (constituents (?verb-phrase)))
                (?verb-phrase
                 --
                 (parent ?subclause)
                 (constituents ?vp-constituents))
                (?cause-phrase
                 --
                 (sem-cat (phrase-type cause-phrase))
                 (constituents (?frame-evoking-unit))
                 (parent ?verb-phrase))
                (?frame-evoking-unit
                 --
                 (sem-frame (causation
                             (effect (unit-append (unit-remove ?that-clause ?top-constituents) 
                                                  ?wh-phrase
                                                  (unit-remove ?cause-phrase ?vp-constituents)))))
                 (parent ?cause-phrase)))
               :cxn-inventory *fcg-english*)

;; Active-Transitive Linking
(def-frame-cxn Active-Transitive-Prepositional-Verb-cxn
               (<-
                (?sentence
                 --
                 (syn-cat (clause-type ?clause-type))
                 (constituents (?subject-phrase ?verb-phrase)))
                (root
                 --
                 (form ((meets ?subject-phrase ?verb-phrase ?sentence))))
                (?verb-phrase
                 --
                 (syn-cat (phrase-type verb-phrase))
                 (hash form ((meets ?root-verb ?prepphrase ?verb-phrase))))
                (?root-verb
                 --
                 (parent ?verb-phrase)
                 (sem-frame (causation
                             (cause ?subject-phrase)
                             (effect ?object-phrase))))
                (?prepphrase
                 --
                 (syn-cat (phrase-type prepositional-phrase))
                 (parent ?verb-phrase)
                 (constituents (?preposition ?object-phrase))
                 (hash form ((meets ?preposition ?object-phrase ?prep-phrase)))))
               :cxn-inventory *fcg-english*)

(def-frame-cxn Active-Transitive-Verb-cxn
               (<-
                (?sentence
                 --
                 (syn-cat (clause-type ?clause-type))
                 (constituents (?subject-phrase ?verb-phrase)))
                (?root-verb
                 --
                 (parent ?verb-phrase)
                 (functional-structure (direct-object ?object))
                 (sem-frame (causation
                             (cause ?subject-phrase)
                             (effect ?object-phrase))))
                (root
                 --
                 (form ((meets ?subject-phrase ?verb-phrase ?sentence))))
                (?object
                 --
                 (parent ?object-phrase)))
               :cxn-inventory *fcg-english*)

;; Passive-linking (two steps):
;; ----------------------------
(def-frame-cxn Passive+by-phrase-cxn
               (<-
                (?root-verb
                 --
                 (syn-cat (lex-class verb))
                 (sem-frame (causation
                             (cause ?cause-phrase)))
                 (functional-structure (agent ?agent)))
                (?agent
                 --
                 (parent ?cause-phrase)))
               :cxn-inventory *fcg-english*)

(def-frame-cxn Passive-Subject-Effect-cxn
               (<-
                (?root-verb
                 --
                 (syn-cat (lex-class verb))
                 (functional-structure (subject ?subject))
                 (sem-frame (causation
                             (effect ?subject-phrase)))
                 (dependents (?passive-aux)))
                (?passive-aux
                 --
                 (syn-cat (is-passive-marker +)))
                (?subject
                 --
                 (parent ?subject-phrase)))
               :cxn-inventory *fcg-english*)
                 

;; NP-that/which-VP-cxn

(def-frame-cxn cause-WH-determiner-effect-cxn
               (<-
                (?root-verb
                 --
                 (syn-cat (lex-class verb))
                 (parent ?verb-phrase)
                 (sem-frame (causation
                             (cause (unit-remove ?subordinate-clause ?np-constituents))
                             (effect ?effect-phrase))))
                (?wh-determiner
                 --
                 (syn-cat (determiner-type wh-determiner))
                 (parent ?wh-phrase)
                 (dep-head ?root-verb))
                (?verb-phrase
                 --
                 (syn-cat (phrase-type verb-phrase))
                 (constituents (?root-verb ?prepphrase)))
                (?prepphrase
                 --
                 (syn-cat (phrase-type prepositional-phrase))
                 (parent ?verb-phrase)
                 (constituents (?preposition ?effect-phrase))
                 (hash form ((meets ?preposition ?effect-phrase ?prepphrase))))
                (?wh-phrase
                 --
                 (syn-cat (phrase-type noun-phrase))
                 (constituents (?wh-determiner))
                 (parent ?subordinate-clause))
                (?subordinate-clause
                 --
                 (syn-cat (clause-type subordinate))
                 (parent ?top-np))
                (?top-np
                 --
                 (syn-cat (phrase-type noun-phrase))
                 (constituents ?np-constituents)))
               :cxn-inventory *fcg-english*)




