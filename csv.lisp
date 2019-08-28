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
  (:use :cl)
  (:export
   #:make-csv-format

   ;; csv formatting
   #:csv-format-comment
   #:csv-format-separator
   #:csv-format-quote
   #:csv-format-escape

   ;; special variables
   #:*csv-format*

   ;; reading functions
   #:read-csv
   #:read-record

   ;; writing functions
   #:write-csv
   #:write-record))

(in-package :csv)

;;; ----------------------------------------------------

(defstruct csv-format
  "Description of a CSV format used for reading/writing."
  (comment #\#)
  (separator #\,)
  (quote #\")
  (escape #\\))

;;; ----------------------------------------------------

(defparameter *csv-format* (make-csv-format)
  "Default CSV format.")

;;; ----------------------------------------------------

(defun read-csv (stream &optional (format *csv-format*))
  "Collect all rows from a stream into a list."
  (loop for row = (read-record stream format) while row collect row))

;;; ----------------------------------------------------

(defun read-record (stream &optional (format *csv-format*))
  "Read the next row of cells into a vector."
  (declare (optimize (speed 3) (debug 0)))
  (loop
     with row = (list nil)
     with tail = row

     ;; test for end of file
     for c = (peek-char nil stream nil)
     until (null c)

     ;; skip comments
     do (if (equal c (csv-format-comment format))
            (read-line stream)
          (loop (multiple-value-bind (cell eol)
                    (read-cell stream format)
                  (setf tail (cdr (rplacd tail (list cell))))
                  (when eol
                    (return-from read-record (cdr row))))))))

;;; ----------------------------------------------------

(defun read-cell (stream format)
  "Read a cell from the current record."
  (declare (optimize (speed 3) (debug 0)))
  (with-slots (separator quote escape)
      format
    (loop
       with cell = (make-string-output-stream)

       ;; get the next character
       for char = (read-char stream nil)
       for endp = (or (equal char nil)
                      (equal char #\return)
                      (equal char #\linefeed))

       ;; stop cell parsing at delimiter or end of line
       when (or (equal char separator) endp)
       return (values (get-output-stream-string cell) endp)

       ;; write character to row cell
       do (if (equal char quote)
              (do ((c (read-char stream)
                      (read-char stream)))
                  ((equal c quote))
                (if (equal c escape)
                    (write-char (read-char stream) cell)
                  (write-char c cell)))
            (write-char char cell)))))

;;; ----------------------------------------------------

(defun write-csv (rows stream &optional (format *csv-format*))
  "Write CSV records to a stream using the provided format."
  (map nil #'(lambda (row) (write-record row stream format)) rows))

;;; ----------------------------------------------------

(defun write-record (record stream &optional (format *csv-format*))
  "Write a single CSV record to a stream using the given format."
  (let ((*csv-format* format))
    (do ((cell (pop record)
               (pop record)))
        ((null cell))
      (write-cell cell stream format)

      ;; delimiter or newline
      (if (null record)
          (terpri stream)
        (write-char (csv-format-separator format) stream)))))

;;; ----------------------------------------------------

(defun write-cell (cell stream format)
  "Format a CSV record cell to a stream."
  (with-slots (separator quote escape)
      format
    (let ((s (princ-to-string cell)))
      (if (find separator s :test #'equal)
          (progn
            (write-char quote stream)

            ;; output all the cell characters to the stream
            (loop
               for c across s

               ;; write this portion of the cell
               do (if (equal c quote)
                      (format stream "~c~c" escape quote)
                    (princ c stream))

               ;; close the string
               finally (write-char quote stream)))
        (princ s stream)))))
