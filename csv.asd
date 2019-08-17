(defpackage :csv-asd
  (:use :cl :asdf))

(in-package :csv-asd)

(defsystem :csv
  :name "csv"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "CSV parsing for Common Lisp."
  :serial t
  :components ((:file "csv")))
