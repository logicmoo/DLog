To use sax_owl2sqlschema.py, set the proper "constants" on the top of
the file. Currently they are set for the included lubm files. 
It will generate three tables (instances,objectproperties and
datatypeproperties), as having URIs for tablenames is not always
possible. (Table name limit is 64 characters for Mysql and Postgresql*
eventhough the SQL standard is 128.    

* In the precompiled binaries. It's possible to compile them for longer
tablenames.

The output files are schema.sql and data.sql(5MB for the lubm example).