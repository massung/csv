;;;; CSV Parsing for Common Lisp
;;;;
;;;; Copyright (c) Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :csv
  (:use :cl :lexer :parse)
  (:export
   #:csv-parse
   #:csv-format))

(in-package :csv)

;;; ----------------------------------------------------

(define-lexer csv-lexer (s)
  ("%n+"      (values :end))
  (","        (values :comma))

  ;; double quotes around a cell
  ("\""       (push-lexer s 'string-lexer :quote))

  ;; anything else is the cell
  (".[^%n,]*" (values :cell $$)))

;;; ----------------------------------------------------

(define-lexer string-lexer (s)
  ("\"\""     (values :chars "\""))

  ;; end of the string?
  ("\""       (pop-lexer s :quote))

  ;; anything else
  (".[^\"]*"  (values :chars $$)))

;;; ----------------------------------------------------

(define-parser csv-parser
  (.sep-by1 'csv-record (.is :end)))

;;; ----------------------------------------------------

(define-parser csv-record
  (.sep-by1 'csv-cell (.is :comma)))

;;; ----------------------------------------------------

(define-parser csv-cell
  (.one-of 'csv-string (.is :cell) (.ret "")))

;;; ----------------------------------------------------

(define-parser csv-string
  (.let (cs (>> (.is :quote) (.many-until (.is :chars) (.is :quote))))
    (.ret (format nil "~{~a~}" cs))))

;;; ----------------------------------------------------

(defun csv-parse (string &optional source)
  "Convert a CSV string into a Lisp object."
  (with-lexer (lexer 'csv-lexer string :source source)
    (with-token-reader (next-token lexer)
      (parse 'csv-parser next-token))))

;;; ----------------------------------------------------

(defun csv-format (record &optional stream)
  "Convert a list of Lisp objects into a list a CSV string."
  (format stream "~{~/csv::format-cell/~^,~}" record))

;;; ----------------------------------------------------

(defun format-cell (stream cell &optional colonp atp &rest args)
  "Format a CSV record cell to a stream."
  (declare (ignore colonp atp args))
  (let ((s (princ-to-string cell)))
    (if (find #\, s)
        (progn
          (write-char #\" stream)

          ;; output all the cell characters to the stream
          (loop
             for c across s

             ;; write this portion of the cell
             do (if (char= c #\")
                    (write-string "\"\"" stream)
                  (princ c stream))

             ;; close the string
             finally (write-char #\" stream)))
      (princ s stream))))
