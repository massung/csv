# CSV Parser for LispWorks

The `csv` package is a dead-simple [Comma Separated Values](http://www.json.org) parser for [LispWorks](http://www.lispworks.com). It adhears to the [RFC 4180](https://tools.ietf.org/html/rfc4180) spec.

## Quickstart

Parsing a CSV is simply a matter of calling `parse-csv`.

	(parse-csv string &optional source) ;=> list

It is always assumes that the CSV is multi-line, so the result is a list of lists, where each inner list is a record of cells.

	CL-USER > (parse-csv "1,2,3")
	(("1" "2" "3"))

Generating a CSV string from Lisp is done with the `format-csv` function.

	(format-csv record &optional stream) ;=> string
	
The *record* parameter should be a list of cells and not a list of records.

	CL-USER > (format-csv '(1 "Hello, world" 2))
	"1,\"Hello, world\",2"
	
That's it!
