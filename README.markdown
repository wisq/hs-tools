hs-tools
========

This is a collection of miscellaneous (but practical) tools I've written in Haskell to solve various problems, while learning the language as well.


schema-reorder
--------------

Given two Rails schema files, file1 and file2, schema-reorder will output file1 with each table's columns sorted to match file2's columns for the equivalent table.

This is useful when trying to resyncronise a development schema.rb with a dump of the production schema, since each may have had migrations applied at different times (and hence different column orders).
