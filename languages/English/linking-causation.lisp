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

(def-frame-cxn subject-vp-because-phrase-cxn
               (<-
                (?sentence
                 --
                 (syn-cat (clause-type ?clause-type))
                 (constituents (?subject-phrase ?because-phrase ?verb-phrase)))
                (?because-phrase
                 --
                 (parent ?sentence)
                 (sem-cat (phrase-type because-phrase))
                 (constituents (?because)))
                (?because
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

(def-frame-cxn subject-vp-incl-because-phrase-cxn
               (<-
                (?sentence
                 --
                 (syn-cat (clause-type ?clause-type))
                 (constituents (?subject-phrase ?verb-phrase))
                 (hash form ((meets ?subject-phrase ?verb-phrase ?sentence))))
                (?verb-phrase
                 --
                 (syn-cat (phrase-type verb-phrase))
                 (constituents ?vp-constituents))
                (?because-phrase
                 --
                 (parent ?verb-phrase)
                 (sem-cat (phrase-type because-phrase))
                 (constituents (?because)))
                (?because
                 --
                 (sem-frame (causation
                             (effect (unit-append ?subject-phrase (unit-remove ?because-phrase ?vp-constituents)))))))
               :cxn-inventory *fcg-english*)

(def-frame-cxn subject-vp-incl-because-phrase-cxn
               (<-
                (?sentence
                 --
                 (syn-cat (clause-type ?clause-type))
                 (constituents (?subject-phrase ?verb-phrase))
                 (hash form ((meets ?subject-phrase ?top-verb-phrase ?sentence))))
                (?top-verb-phrase
                 --
                 (syn-cat (phrase-type verb-phrase))
                 (constituents (?aux ?verb-phrase)))
                (?verb-phrase
                 --
                 (syn-cat (phrase-type verb-phrase))
                 (constituents ?vp-constituents))
                (?because-phrase
                 --
                 (parent ?verb-phrase)
                 (sem-cat (phrase-type because-phrase))
                 (constituents (?because)))
                (?because
                 --
                 (sem-frame (causation
                             (effect (unit-append ?subject-phrase ?aux (unit-remove ?because-phrase ?vp-constituents)))))))
               :cxn-inventory *fcg-english*)

(def-frame-cxn subject-that-VP-incl-because-phrase-cxn
               (<-
                (?top-noun-phrase
                 --
                 (hash form ((meets ?subject-phrase ?that-clause ?top-noun-phrase)))
                 (syn-cat (phrase-type noun-phrase))
                 (constituents (?subject-phrase ?that-clause)))
                (?that-clause
                 --
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
                (?because-phrase
                 --
                 (sem-cat (phrase-type because-phrase))
                 (constituents (?because))
                 (parent ?verb-phrase))
                (?because
                 --
                 (sem-frame (causation
                             (effect (unit-append ?subject-phrase ?wh-phrase (unit-remove ?because-phrase ?vp-constituents)))))
                 (parent ?because-phrase)))
               :cxn-inventory *fcg-english*)