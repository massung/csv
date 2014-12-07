(defpackage :csv-asd
  (:use :cl :asdf))

(in-package :csv-asd)

(defsystem :csv
  :name "csv"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "CSV Parsing for LispWorks."
  :serial t
  :components ((:file "csv"))
  :depends-on ("lexer"))
