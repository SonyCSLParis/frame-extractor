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
                 (sem-cat (phrase-type because-phrase))
                 --
                 (hash form ((meets ?because ?cause-phrase ?because-phrase)))
                 (constituents (?because ?cause-phrase)))
                (?cause-phrase
                 --
                 (syn-cat (not (lex-class ?lex-class)))))
               :attributes (:label :hashed-string
                            :string "because")
               :cxn-inventory *fcg-english*)

(def-frame-cxn because-of-lex
               (<-
                (?because
                 (sem-frame (causation
                             (cause ?cause-phrase)))
                 (lex-id because)
                 --
                 (hash form ((string ?because "because")
                             (string ?of "of")))
                 (parent ?because-phrase))
                (?because-phrase
                 (sem-cat (phrase-type because-phrase))
                 --
                 (hash form ((meets ?because ?of ?because-phrase)
                             (meets ?of ?cause-phrase ?because-phrase)))
                 (constituents (?because ?of ?cause-phrase))))
               :attributes (:label :hashed-string
                            :string "because")
               :cxn-inventory *fcg-english*)


