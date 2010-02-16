from xml.sax import make_parser
from xml.sax.handler import ContentHandler
from xml.sax.handler import feature_namespaces
import os
#os.path.abspath(os.curdir)

writenamespace = True
aboxfile = 'University0_0.owl'
sqldatafilename = 'data.sql'
sqlschemafilename = 'schema.sql'

def toAbsolutePath(lpath):
    return os.path.abspath(os.curdir) + os.sep + lpath

sqldatafilename = toAbsolutePath(sqldatafilename)
sqlschemafilename = toAbsolutePath(sqlschemafilename)

fhdata = file(sqldatafilename,'w')
fhschema = file(sqlschemafilename,'w')

rdfresource = (u'http://www.w3.org/1999/02/22-rdf-syntax-ns#', u'resource')
rdfabout = (u'http://www.w3.org/1999/02/22-rdf-syntax-ns#', u'about')

class OwlObject:
    datatypeprop = {}
    objprop = {}
    classname = ''
    uri = ''


class OwlClass:
    classname = ''
    datatypeprop = set()
    objprop = set()
    def __init__(self,owlobj):
        self.classname = owlobj.classname
    def __str__(self):
        return unifyName( self.classname )
    def update(self,owlobj):
        if self.classname != owlobj.classname:
            raise Exception('update with object of invalid class attempted')
        self.datatypeprop.update(set(owlobj.datatypeprop.keys()))
        self.objprop.update(set(owlobj.objprop.keys()))
        
    

# if you uncomment (1), objects will be stored here (this might need a lot of
# memory)
owlobjects = []


# This hashmap stores which class has what attributes. Used for SQL schema generation.
# Key: classname, Value: list of attribute names (both are tuples with appropriate namespaces) 
# e.g.: classtemplates[(u'http://www.lehigh.edu/~zhp2/2004/0401/univ-bench.owl#', u'Publication')]
classtemplates = {}

def unifyName(nsname):
    (n1,n2) = nsname
    return n1+n2    

testatt = None
#imhere
fhdata.write("""
delete from datatypeproperty;
delete from instances;
delete from objectproperty;
""")

def writeObject(owlobject):
    if not owlobject.classname == (u'http://www.w3.org/2002/07/owl#', u'Ontology'):
        fhdata.write("""
INSERT INTO instances (subject,classname)
    VALUES('"""+ owlobject.uri +"','"+ unifyName(owlobject.classname) +"');")
        for op in owlobject.objprop:
            if not op == (u'http://www.w3.org/2002/07/owl#',u'imports'):
##                print "op: ",op                
                fhdata.write("""
INSERT INTO objectproperty (subject,object,objpropname )
    VALUES('"""+ owlobject.uri +"','"+  owlobject.objprop[op] +"','"+ unifyName(op) +"');")
            pass
        for dtp in owlobject.datatypeprop:
            fhdata.write("""
INSERT INTO datatypeproperty (subject,value,dtpropname)
    VALUES('"""+ owlobject.uri +"','"+  owlobject.datatypeprop[dtp] +"','"+ unifyName(dtp) +"');")
            pass
        pass


def writeSchema():
    fhschema.write("""
drop database dlog;
create database dlog;
use dlog;
CREATE TABLE instances (
    id bigint unsigned not null auto_increment,
    primary key(id),
    subject text,
    object text,
    classname text
);

# objectprop
CREATE TABLE objectproperty (
    id bigint unsigned not null auto_increment,
    primary key(id),
    subject text,
    object text,
    objpropname text
);


# datatypeprop
CREATE TABLE datatypeproperty (
    id bigint unsigned not null auto_increment,
    primary key(id),
    subject text,
    value text,
    dtpropname text
);

""")    
    pass # end writeSchema


class OwlAboxHandler(ContentHandler):
    """
Sax handler for creating SQL dump from OWL Abox files.

Implementation details:

In LUBM (and hopefully in other owl Aboxes as well) objects are
on the second level of XML tree, properties/relations on the third
level, inline object definitions (i.e. objects which are only
referenced once, and don't appear on second level)
are on the fourth level. We keep track of the current level with 'xmlstack'.
When we leave a second level (closing tag for objects),
we write the object to the sqldump.
    """
    xmlstack = []
    currentObject = None
    # owl attribute
    currentAttribute = None
    
    def startElementNS(self,name,qname,attribute):
        self.xmlstack.append(name)
        uname = unifyName(name)
        if len(self.xmlstack) == 2:
            """owlclass instance"""
            self.currentObject = OwlObject()
            self.currentObject.classname = name
            self.currentObject.uri = attribute.getValue(rdfabout)
            
# (1)
##            global owlobjects
##            owlobjects.append(self.currentObject)
# (1) end
            pass
        elif len(self.xmlstack) == 3:
            """attribute or relation"""
            self.currentAttribute = name
            if rdfresource in attribute.getNames():
                """relation aka OjectProperty"""
                self.currentObject.objprop.update({name:attribute.getValue(rdfresource)})
                pass
            else:
                pass
            pass
        elif len(self.xmlstack) == 4:
            """inline class instance definition aka ObjectProperty"""
            self.currentObject.objprop.update({self.currentAttribute:attribute.getValue(rdfabout)})
            pass
        elif len(self.xmlstack) > 4:
            """ In LUBM it's not expected to have higher nesting-level"""
            raise Exception("nested level > 4 ; "+str(self.xmlstack))

    def characters(self, content):
        if content.strip() != "":
             if len(self.xmlstack) == 3:
                 """datatype property declaration"""
                 self.currentObject.datatypeprop.update({self.currentAttribute:content.strip()})                 

    def processOwlObject(self,obj):
        global classtemplates
        # union of attribute sets
        if not classtemplates.has_key(obj.classname):
            classtemplates[obj.classname] = OwlClass(obj)
        classtemplates[obj.classname].update(obj)            
        writeObject(obj)

    def endElementNS(self,name,qname):
        if not self.xmlstack.pop() == name:
            raise Exception('Non proper nesting of tags.')
        if len(self.xmlstack) == 1:
            self.processOwlObject(self.currentObject)
            pass
                                  

parser = make_parser()
parser.setFeature(feature_namespaces, 1)

parser.setContentHandler(OwlAboxHandler())
#parser.setContentHandler(TesterHandler())

parser.parse(aboxfile)
writeSchema()

fhschema.close()
fhdata.close()
