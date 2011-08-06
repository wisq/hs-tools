hs-tools
========

This is a collection of miscellaneous (but practical) tools I've written in Haskell to solve various problems, while learning the language as well.


schema-reorder
--------------

Given two Rails schema files, file1 and file2, schema-reorder will output file1 with each table's columns sorted to match file2's columns for the equivalent table.

This is useful when trying to resyncronise a development schema.rb with a dump of the production schema, since each may have had migrations applied at different times (and hence different column orders).

mod-repack
----------

Given a list of archives (ZIP/RAR/7Zip), mod-repack will extract each one to a temporary directory.  If the result is a single directory, it will recompress the archive (with extension "-repack.7z"), omitting the top-level directory.

This was designed to deal with mods for "The Elder Scrolls: Oblivion" and the mod installer tool "Wrye Bash", which doesn't recognise mod archives unless the data directories are at the top level of the archive.

(Obviously this tool could fail faster if it listed the contents before extracting them to see if they match expectations, but given that it deals with three different archivers with different output formats, it's a lot of extra work to do so.)
