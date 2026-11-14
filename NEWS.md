# dbparser.mcp 0.1.0

## Filters for read_table and export_csv

* `read_table` and `export_csv` take an optional `filters` argument: a list of
  `{column, value}`. A row is kept when the column equals the value exactly
  (as text, case-sensitive, no partial match).
* A value may be a list of up to 100 values; a row matches if it equals any of
  them. Filters on different columns combine with AND.
* Column names are checked exactly; an unknown column, an empty value list or
  a list over 100 values is an error with a next step.
* With filters, `total_rows` counts the matching rows, and a note states the
  filter used, e.g. "Filter: symbol in (PDCD1, SNCA, SPATA2). 3 of 3 matching
  row(s) returned." `export_csv` writes only the matching rows and says so.
