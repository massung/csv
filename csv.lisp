;;;; CSV Parsing for LispWorks
;;;;
;;;; Copyright (c) 2014 by Jeffrey Massung
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
  (:use :cl :lw :lexer :parsergen)
  (:export
   #:parse-csv
   #:decode-csv))

(in-package :csv)

(deflexer csv-lexer
  ("%n+"                (values :end))
  (","                  (values :comma))

  ;; double quotes around a cell
  ("\""                 (push-lexer 'string-lexer :string))

  ;; anything else is the cell
  (".[^%n,]*"           (values :cell (string-trim '(#\space #\tab) $$))))

(deflexer string-lexer
  ("\"\""               (values :chars "\""))

  ;; end of the string?
  ("\""                 (pop-lexer :string))

  ;; anything else
  (".[^\"]*"            (values :chars $$)))

(defparser csv-parser
  ((csv records) $1)

  ;; a list of records
  ((records record records) `(,$1 ,@$2))
  ((records))

  ;; a record is comma-separated cells
  ((record cell :comma record) `(,$1 ,@$3))
  ((record cell :end) `(,$1))
  ((record cell) `(,$1))

  ;; a cell is a quoted string or a set of characters
  ((cell :cell) $1)
  ((cell :string string) $2)
  ((cell :error) "")

  ;; a quoted cell value
  ((string :chars string) (string-append $1 $2))
  ((string :string) ""))

(defun parse-csv (string &optional source)
  "Convert a CSV string into a Lisp object."
  (parse #'csv-parser (tokenize #'csv-lexer string source)))

