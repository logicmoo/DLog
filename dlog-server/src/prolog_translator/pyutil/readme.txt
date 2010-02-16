To use sax_owl2sqlschema.py, set the proper "constants" on the top of
the file. Currently they are set for the included lubm files. 
It will create a new database called "dlog"* and generate three tables
(instances,objectproperties and datatypeproperties), as having URIs for
tablenames is not always possible. (Table name limit is 64 characters
for Mysql and Postgresql** eventhough the SQL standard is 128.)    

* and deletes the one if currently exists
** In the precompiled binaries. It's possible to compile them for longer
tablenames.


The output files are schema.sql and data.sql(5MB for the lubm example).
The data.sql cannot be loaded by the Mysql workbench (5.2.15) due its
size (it causes crash), it must be imported from the command line e.g.:
mysql -u root -p dlog < data.sql