# CSV Parser for Common Lisp

The `csv` package is a dead-simple [Comma Separated Values](https://en.wikipedia.org/wiki/Comma-separated_values) parser for Common Lisp. It is fast, customizable, and works on streams.

## Quickstart

Since there are so many versions of the CSV file, the first thing to do is define the `csv-format` you'd like to use with `make-csv-format`:

    CL-USER > (make-csv-format :separator '(#\tab))
    #S(CSV::CSV-FORMAT :COMMENT #\# :SEPARATOR (#\Tab) :QUOTE #\" :ESCAPE #\\)

Now, it's possible to use that format with one of the read/write CSV functions available.

Read functions:

    (read-csv stream &optional format)
    (read-record stream &optional format)

Write functions:

    (write-csv records stream &optional format)
    (write-record record stream &optional format)

There is a global, dynamic format `*csv-format*` that is used if a format is not provided (which can obviously be overridden lexically).

## Example Usage

Parsing a single record:

    CL-USER > (with-input-from-string (s "1,2,\"Hello, world!\",NA")
                (read-record s))
    ("1" "2" "Hello, world!" "NA")

Writing a single record:

    CL-USER > (write-record * *standard-output*)
    1,2,"Hello, world!",NA

That's it!
