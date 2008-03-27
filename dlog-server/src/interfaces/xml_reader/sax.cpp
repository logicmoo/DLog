#include <vector>
#include <exception>
#include <stdexcept>
/* #include <limits> */ /* isn't available yet */
#include <climits>
#include <memory>
#include <sicstus/sicstus.h>
#include <xercesc/util/NameIdPool.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/BinInputStream.hpp>
#include <xercesc/validators/common/ContentSpecNode.hpp>
#include <xercesc/validators/DTD/DTDValidator.hpp>
#include <xercesc/sax2/DefaultHandler.hpp>
#include <xercesc/sax2/XMLReaderFactory.hpp>
#include <xercesc/sax2/SAX2XMLReader.hpp>
#include <xercesc/sax2/Attributes.hpp>
#include <xercesc/framework/LocalFileInputSource.hpp>

class PrologException: public xercesc::SAXException
{
public:
  PrologException(SP_term_ref e): ex(e) {}
  SP_term_ref getException() const {return ex;}

private:
  SP_term_ref ex;
};

class PrologFailure: public xercesc::SAXException {};

// unsigned const MAX_BUF = std::numeric_limits<unsigned>::max();
unsigned const MAX_BUF = UINT_MAX;

char const *prolog_encoding(XMLCh const * const chars,
                            unsigned length = MAX_BUF)
{
  if (chars == 0)
  {
    return "";
  }
  static size_t bufsize = 512;
  static char *convert_buf = static_cast<char *>(SP_malloc(bufsize));
  size_t ci = 0;
  for (unsigned i = 0; i < length && chars[i]; ++i)
  {
    if (ci+WCI_MAX_BYTES >= bufsize)
    {
      bufsize *= 2;
      convert_buf = static_cast<char *>(SP_realloc(convert_buf, bufsize));
    }
    int l = SP_code_wci(convert_buf+ci, chars[i]);
    if (l != -1) ci += l;
    else SP_fprintf(SP_stderr, "character outside the wide character range\n");
  }
  convert_buf[ci] = '\0';
  return convert_buf;
}

class PrologHandler: public xercesc::DefaultHandler
{
public:
  PrologHandler(SP_term_ref *content, SP_term_ref messages, SP_pred_ref pred,
                bool nsexp, bool nsinh, bool wsfilt, bool noenvcomp,
                bool saxmode, bool listcontent);
  ~PrologHandler();

  void startElement(XMLCh const * const uri,
		    XMLCh const * const localname,
		    XMLCh const * const qname,
		    xercesc::Attributes const &attributes);
  void endElement(XMLCh const * const uri,
		  XMLCh const * const localname,
		  XMLCh const * const qname);
  void characters(XMLCh const * const chars,
		  unsigned const length);

  void warning(xercesc::SAXParseException const &exception);
  void error(xercesc::SAXParseException const &exception);
  void fatalError(xercesc::SAXParseException const &exception);

private:
  typedef std::vector<SP_term_ref> termvec;
  // This is a stack of Prolog variables receiving a list of elements.
  termvec conts;

  // This a Prolog variable receiving the list of messages or the
  // state variable when working in sax_mode.
  SP_term_ref msgs;

  // The predicate to call if working in sax_mode.
  SP_pred_ref callback;

  // Temporary termrefs.
  enum {NIL_TERM_REF=0,START_ELEMENT_ELEM,START_ELEMENT_NAME,
        END_ELEMENT_ELEM,CHARACTERS_ELEM,
        PUT_MESSAGE_FILE,PUT_MESSAGE_LINE,PUT_MESSAGE_COLUMN,PUT_MESSAGE_MSG,
        ELEMENT_ATTR_RES,ELEMENT_ATTR_HEAD,ELEMENT_ATTR_NAME,
        SCOPED_RES,SCOPED_URIREF,CALL_PROLOG_TMP,PROLOG_NAME_RES,
        PROLOG_STRING_RES,PROLOG_STRING_HEAD,PUT_ON_LIST_ELEM,
        TMP_TERM_REF_LAST};     // the last constant is not allocated!
  SP_term_ref tmp_term_refs[TMP_TERM_REF_LAST];

  // Cache the most frequent atoms, so that lookup is not needed.
  SP_atom start_element_atom, end_element_atom, characters_atom;
  SP_atom env_atom, equals_atom, colon_atom;

  // Flags.
  bool namespace_expansion, namespace_inheritance;
  bool whitespace_filter, no_env_compression, sax_mode, list_content;

  // Put a message at the end of the msgs list.
  void putMessage(char const *type, xercesc::SAXParseException const &e);
  SP_term_ref element_attr(xercesc::Attributes const &attributes,
                           SP_term_ref const *parentname);
  SP_term_ref scoped_name(XMLCh const * const uri,
                          XMLCh const * const localname,
                          XMLCh const * const qname,
                          SP_term_ref const *parentname);
  SP_term_ref scoped(bool hasns,
                     XMLCh const * const uri,
                     XMLCh const * const localname,
                     SP_term_ref const *parentname,
                     unsigned urilen = MAX_BUF);
  void call_prolog(SP_term_ref input);
  SP_term_ref prolog_name(XMLCh const * const name, unsigned length = MAX_BUF);
  SP_term_ref prolog_string(XMLCh const *chars, unsigned length, bool clist);
  void put_on_list(SP_term_ref term, SP_term_ref *plist);
};

PrologHandler::PrologHandler(SP_term_ref *content, SP_term_ref messages,
                             SP_pred_ref pred,
                             bool nsexp, bool nsinh, bool wsfilt,
                             bool noenvcomp, bool saxmode, bool listcontent):
  msgs(messages),
  callback(pred),
  start_element_atom(SP_atom_from_string("start_element")),
  end_element_atom(SP_atom_from_string("end_element")),
  characters_atom(SP_atom_from_string("characters")),
  env_atom(SP_atom_from_string("env")),
  equals_atom(SP_atom_from_string("=")),
  colon_atom(SP_atom_from_string(":")),
  namespace_expansion(nsexp),
  namespace_inheritance(nsinh),
  whitespace_filter(wsfilt),
  no_env_compression(noenvcomp),
  sax_mode(saxmode),
  list_content(listcontent)
{
  for (int i = 0; i < TMP_TERM_REF_LAST; ++i)
  {
    tmp_term_refs[i] = SP_new_term_ref();
  }

  if (sax_mode)
  {
    *content = messages;
  }
  else
  {
    SP_term_ref cont = SP_new_term_ref();
    *content = cont;
    SP_put_variable(cont);
    conts.push_back(cont);
  }
}

PrologHandler::~PrologHandler()
{
  if (!sax_mode) 
  {
    for (termvec::iterator i = conts.begin(); i != conts.end(); ++i)
      SP_unify(*i, tmp_term_refs[NIL_TERM_REF]);
    SP_unify(msgs, tmp_term_refs[NIL_TERM_REF]);
  }
}

void PrologHandler::startElement(XMLCh const * const uri,
				 XMLCh const * const localname,
				 XMLCh const * const qname,
				 xercesc::Attributes const &attributes)
{
  SP_term_ref elem = tmp_term_refs[START_ELEMENT_ELEM];
  SP_term_ref name = tmp_term_refs[START_ELEMENT_NAME];
  SP_put_term(name, scoped_name(uri, localname, qname, 0));
  if (sax_mode)
  {
    SP_cons_functor(elem, start_element_atom, 2,
                    name, element_attr(attributes, &name));
    call_prolog(elem);
  }
  else 
  {
    SP_term_ref cont = SP_new_term_ref();
    SP_put_variable(cont);
    if (no_env_compression || 0 < attributes.getLength())
      SP_cons_functor(elem, env_atom, 3, name,
                      element_attr(attributes, &name),
                      cont);
    else
      SP_cons_functor(elem, env_atom, 2, name, cont);
    put_on_list(elem, &conts.back());
    conts.push_back(cont);
  }
}

void PrologHandler::endElement(XMLCh const * const uri,
			       XMLCh const * const localname,
			       XMLCh const * const qname)
{
  if (sax_mode) 
  {
    SP_term_ref elem = tmp_term_refs[END_ELEMENT_ELEM];
    SP_term_ref name = scoped_name(uri, localname, qname, 0);
    SP_cons_functor(elem, end_element_atom, 1, name);
    call_prolog(elem);
  }
  else 
  {
    SP_unify(conts.back(), tmp_term_refs[NIL_TERM_REF]); // terminate the list
    conts.pop_back();
  }
}

void PrologHandler::characters(XMLCh const * const chars,
			       unsigned const length)
{
  if (whitespace_filter && xercesc::XMLString::isAllWhiteSpace(chars)) return;
  SP_term_ref prolog_chars = prolog_string(chars, length, list_content);
  if (sax_mode)
  {
    SP_term_ref elem = tmp_term_refs[CHARACTERS_ELEM];
    SP_cons_functor(elem, characters_atom, 1, prolog_chars);
    call_prolog(elem);
  }
  else 
  {
    put_on_list(prolog_chars, &conts.back());
  }
}

void PrologHandler::fatalError(xercesc::SAXParseException const &e)
{
  putMessage("fatal_error", e);
}

void PrologHandler::error(xercesc::SAXParseException const &e)
{
  putMessage("error", e);
}

void PrologHandler::warning(xercesc::SAXParseException const &e)
{
  putMessage("warning", e);
}

void PrologHandler::putMessage(char const *type,
                               xercesc::SAXParseException const &e)
{
  SP_term_ref line = tmp_term_refs[PUT_MESSAGE_LINE];
  SP_put_integer(line, e.getLineNumber());
  SP_term_ref column = tmp_term_refs[PUT_MESSAGE_COLUMN];
  SP_put_integer(column, e.getColumnNumber());
  SP_term_ref file = tmp_term_refs[PUT_MESSAGE_FILE];
  SP_put_term(file, prolog_name(e.getSystemId()));
  SP_term_ref msg = tmp_term_refs[PUT_MESSAGE_MSG];
  SP_cons_functor(msg, SP_atom_from_string(type), 4, file, line, column,
		  prolog_name(e.getMessage()));
  if (sax_mode)
  {
    call_prolog(msg);
  }
  else
  {
    put_on_list(msg, &msgs);
  }
}

SP_term_ref PrologHandler::element_attr(xercesc::Attributes const &attributes,
                                        SP_term_ref const *parentname)
{
  unsigned i = attributes.getLength();
  SP_term_ref res = tmp_term_refs[ELEMENT_ATTR_RES];
  SP_put_term(res, tmp_term_refs[NIL_TERM_REF]);
  SP_term_ref head = tmp_term_refs[ELEMENT_ATTR_HEAD];
  while (0 < i--)
  {
    SP_term_ref name = tmp_term_refs[ELEMENT_ATTR_NAME];
    SP_put_term(name, scoped_name(attributes.getURI(i),
                                  attributes.getLocalName(i),
                                  attributes.getQName(i),
                                  parentname));
    SP_term_ref value = prolog_name(attributes.getValue(i));
    SP_cons_functor(head, equals_atom, 2, name, value);
    SP_cons_list(res, head, res);
  }
  return res;
}

inline SP_term_ref PrologHandler::scoped_name(XMLCh const * const uri,
                                              XMLCh const * const localname,
                                              XMLCh const * const qname,
                                              SP_term_ref const *parentname)
{
  if (namespace_expansion)
    return scoped(!xercesc::XMLString::isAllWhiteSpace(uri), uri,
                  localname, parentname);
  int i = xercesc::XMLString::indexOf(qname, xercesc::chColon);
  return scoped(i >= 0, qname, qname+i+1, parentname, i);
}

SP_term_ref PrologHandler::scoped(bool hasns,
                                  XMLCh const * const uri,
                                  XMLCh const * const localname,
                                  SP_term_ref const *parentname,
                                  unsigned urilen)
{
  if (hasns || namespace_inheritance && parentname)
  {
    SP_term_ref res = tmp_term_refs[SCOPED_RES];
    SP_term_ref uriref = tmp_term_refs[SCOPED_URIREF];
    if (!hasns)
    {
      SP_get_arg(1, *parentname, uriref);
    }
    else
    {
      SP_put_term(uriref, prolog_name(uri, urilen));
    }
    SP_cons_functor(res, colon_atom, 2,
                    uriref, prolog_name(localname));
    return res;
  }
  return prolog_name(localname);
}

void PrologHandler::call_prolog(SP_term_ref input)
{
  SP_term_ref tmp = tmp_term_refs[CALL_PROLOG_TMP];
  SP_put_variable(tmp);
  switch (SP_query(callback, input, msgs, tmp))
  {
  case SP_SUCCESS:
    SP_put_term(msgs, tmp);
    break;
  case SP_ERROR:
    SP_exception_term(tmp);
    throw PrologException(tmp);
  default:
    throw PrologFailure();
  }
}

// This is a dangerous method.  Since it always returns the same term ref,
// The returned result should be copied before it is called again.
// It should take the term ref to update as parameter, but the current way
// is too convenient when only one Prolog name is needed.
SP_term_ref PrologHandler::prolog_name(XMLCh const * const name,
                                       unsigned length)
{
  SP_term_ref res = tmp_term_refs[PROLOG_NAME_RES];
  SP_put_string(res, prolog_encoding(name, length));
  return res;
}

SP_term_ref PrologHandler::prolog_string(XMLCh const *chars, unsigned length,
                                         bool clist)
{
  SP_term_ref res = tmp_term_refs[PROLOG_STRING_RES];
  if (clist) {
      SP_put_term(res, tmp_term_refs[NIL_TERM_REF]);
      SP_term_ref head = tmp_term_refs[PROLOG_STRING_HEAD];
      while (0 < length--)
      {
        SP_put_integer(head, chars[length]);
        SP_cons_list(res, head, res);
      }
  }
  else 
  {
    SP_put_string(res, prolog_encoding(chars, length));
  }
  return res;
}

// Put term at the end of plist.
void PrologHandler::put_on_list(SP_term_ref term, SP_term_ref *plist)
{
  SP_term_ref tail = SP_new_term_ref();
  SP_put_variable(tail);
  SP_term_ref elem = tmp_term_refs[PUT_ON_LIST_ELEM];
  SP_cons_list(elem, term, tail);
  SP_unify(*plist, elem);
  *plist = tail;
}

void raise_exception(char const *msg)
{
  SP_term_ref t1 = SP_new_term_ref(), t2 = SP_new_term_ref();
  SP_put_string(t2, msg);
  SP_cons_functor(t1, SP_atom_from_string("xml_error"), 1, t2);
  SP_raise_exception(t1);
}

SP_term_ref content_model(xercesc::ContentSpecNode const *node)
{
  SP_term_ref model = SP_new_term_ref();
  switch (node->getType())
  {
  case xercesc::ContentSpecNode::Leaf:
    if (node->getElement()->getURI() == xercesc::XMLElementDecl::fgPCDataElemId)
      SP_put_string(model, prolog_encoding(xercesc::XMLElementDecl::fgPCDataElemName));
    else
      SP_put_string(model, prolog_encoding(node->getElement()->getRawName()));
    break;
  case xercesc::ContentSpecNode::ZeroOrOne:
    SP_cons_functor(model, SP_atom_from_string("?"), 1,
                    content_model(node->getFirst()));
    break;
  case xercesc::ContentSpecNode::ZeroOrMore:
    SP_cons_functor(model, SP_atom_from_string("*"), 1,
                    content_model(node->getFirst()));
    break;
  case xercesc::ContentSpecNode::OneOrMore:
    SP_cons_functor(model, SP_atom_from_string("+"), 1,
                    content_model(node->getFirst()));
    break;
  case xercesc::ContentSpecNode::Choice:
    SP_cons_functor(model, SP_atom_from_string("|"), 2,
                    content_model(node->getFirst()),
                    content_model(node->getSecond()));
    break;
  case xercesc::ContentSpecNode::Sequence:
    {
      SP_term_ref tail = content_model(node->getSecond());
      if (!SP_is_list(tail))
        SP_cons_list(tail, tail, SP_new_term_ref());
      SP_cons_list(model, content_model(node->getFirst()), tail);
    }
    break;
  default:
    throw std::logic_error("unexpected node type in DTD");
  }
  return model;
}

SP_term_ref dtd_att(xercesc::XMLAttDef const &attDef)
{
  SP_term_ref type = SP_new_term_ref();
  switch (attDef.getType())
  {
  case xercesc::XMLAttDef::CData:
    SP_put_string(type, "cdata");
    break;
  case xercesc::XMLAttDef::ID:
    SP_put_string(type, "id");
    break;
  case xercesc::XMLAttDef::IDRef:
    SP_put_string(type, "idref");
    break;
  case xercesc::XMLAttDef::IDRefs:
    SP_put_string(type, "idrefs");
    break;
  case xercesc::XMLAttDef::Entity:
    SP_put_string(type, "entity");
    break;
  case xercesc::XMLAttDef::Entities:
    SP_put_string(type, "entities");
    break;
  case xercesc::XMLAttDef::NmToken:
    SP_put_string(type, "nmtoken");
    break;
  case xercesc::XMLAttDef::NmTokens:
    SP_put_string(type, "nmtokens");
    break;
  case xercesc::XMLAttDef::Notation:
    SP_put_string(type, "notation");
    break;
  case xercesc::XMLAttDef::Enumeration:
    SP_put_string(type, "enumeration");
    break;
  default:
    throw std::logic_error("unexpected attribute type in DTD");
  }
  SP_term_ref att = SP_new_term_ref();
  SP_term_ref name = SP_new_term_ref();
  SP_put_string(name, prolog_encoding(attDef.getFullName()));
  SP_cons_functor(att, SP_atom_from_string("attr"), 2, name, type);
  return att;
}

bool dtd_entities(xercesc::SAX2XMLReader *parser, SP_term_ref dtd)
{
  xercesc::XMLValidator *validator = parser->getValidator();
  if (!validator) return false;
#ifdef HAVE_RTTI
  xercesc::DTDGrammar *grammar = dynamic_cast<xercesc::DTDGrammar*>(validator->getGrammar());
#else
  xercesc::DTDGrammar *grammar = (xercesc::DTDGrammar*)(validator->getGrammar());
#endif
  if (!grammar) return false;
  xercesc::NameIdPoolEnumerator<xercesc::DTDElementDecl> elemEnum = grammar->getElemEnumerator();
  while (elemEnum.hasMoreElements())
  {
    xercesc::DTDElementDecl const &curElem = elemEnum.nextElement();
    SP_term_ref name = SP_new_term_ref();
    SP_put_string(name, prolog_encoding(curElem.getFullName()));
    SP_term_ref cm;
    switch (curElem.getModelType())
    {
    case xercesc::DTDElementDecl::Any:
      cm = SP_new_term_ref();
      SP_put_string(cm, "any");
      break;
    case xercesc::DTDElementDecl::Empty:
      cm = SP_new_term_ref();
      SP_put_string(cm, "empty");
      break;
    default:
      cm = content_model(curElem.getContentSpec());
    } 
    SP_term_ref elem = SP_new_term_ref();
    if (curElem.hasAttDefs())
    {
      xercesc::XMLAttDefList &attList = curElem.getAttDefList();
      SP_term_ref atts = SP_new_term_ref();
      while (attList.hasMoreElements())
        SP_cons_list(atts, dtd_att(attList.nextElement()), atts);
      SP_cons_functor(elem, SP_atom_from_string("entity"), 3, name, atts, cm);
    }
    else
      SP_cons_functor(elem, SP_atom_from_string("entity"), 2, name, cm);
    SP_cons_list(dtd, elem, dtd);
  }
  return true;
}

class InitGuard
{
public:
  InitGuard() {xercesc::XMLPlatformUtils::Initialize();}
  ~InitGuard() {xercesc::XMLPlatformUtils::Terminate();}
};

#ifndef WIN32
template <typename Arg, Arg (*fun)(Arg)>
class ScopedHandler
{
public:
  ScopedHandler(Arg a): prev_arg(fun(a)) {}
  ~ScopedHandler() {fun(prev_arg);}

private:
  Arg prev_arg;
};
#else
class ScopedHandler
{
public:
  ScopedHandler(unexpected_handler a): prev_arg(set_unexpected(a)) {}
  ~ScopedHandler() {set_unexpected(prev_arg);}

private:
  unexpected_handler prev_arg;
};
#endif

void map_unexpected()
{
  SP_printf("unexpected exception\n");
  throw std::runtime_error("unexpected exception");
}

enum sax_flags {VALIDATE              = 1 << 0,
                DYNAMIC_VALIDATION    = 1 << 1,
                NAMESPACE             = 1 << 2,
                NAMESPACE_PREFIX      = 1 << 3,
                NAMESPACE_EXPANSION   = 1 << 4,
                SCHEMA                = 1 << 5,
                SCHEMA_FULL_CHECK     = 1 << 6,
                DTD_PROCESSING        = 1 << 7,
                WHITESPACE_FILTER     = 1 << 8,
                NO_ENV_COMPRESSION    = 1 << 9,
                NAMESPACE_INHERITANCE = 1 << 10,
                SAX_MODE              = 1 << 11,
                LIST_CONTENT          = 1 << 12};

SP_term_ref parseXML(xercesc::InputSource const &source,
		     long flags,
		     SP_term_ref messages, // or startState
		     SP_term_ref dtd,
		     SP_atom predName,
		     SP_atom module)
{
  SP_term_ref content = 0;
  SP_pred_ref pred = 0;
  bool saxMode = flags&SAX_MODE;
  if (saxMode)
  {
    pred = SP_pred(predName, 3L, module);
    if (!pred)
    {
      raise_exception("hook predicate not found");
      return content;
    }
  }

  PrologHandler handler(&content, messages, pred,
			flags&NAMESPACE_EXPANSION,
			flags&NAMESPACE_INHERITANCE,
			flags&WHITESPACE_FILTER,
			flags&NO_ENV_COMPRESSION,
			saxMode,
			flags&LIST_CONTENT);
  std::auto_ptr<xercesc::SAX2XMLReader>
    parser(xercesc::XMLReaderFactory::createXMLReader());
  parser->setFeature(xercesc::XMLUni::fgSAX2CoreValidation, flags&VALIDATE);
  parser->setFeature(xercesc::XMLUni::fgXercesDynamic, flags&DYNAMIC_VALIDATION);
  parser->setFeature(xercesc::XMLUni::fgSAX2CoreNameSpaces, flags&NAMESPACE);
  parser->setFeature(xercesc::XMLUni::fgSAX2CoreNameSpacePrefixes,
		     flags&NAMESPACE_PREFIX);
  parser->setFeature(xercesc::XMLUni::fgXercesSchema, flags&SCHEMA);
  parser->setFeature(xercesc::XMLUni::fgXercesSchemaFullChecking,
		     flags&SCHEMA_FULL_CHECK);
  parser->setContentHandler(&handler);
  parser->setErrorHandler(&handler);
  parser->parse(source);
  if (flags&DTD_PROCESSING) dtd_entities(parser.get(), dtd);

  return content;
}

XMLCh *xerces_encoding(char *prologString, std::vector<XMLCh> &buf)
{
  int charCount = 0, charLength;
  char *wci = prologString;
  while ((charLength = SP_wci_len(wci)) > 0)
  {
    wci += charLength;
    ++charCount;
  }
  buf.clear();
  buf.reserve(charCount+1);
  wci = prologString;
  int charCode;
  while ((charLength = SP_wci_code(&charCode, wci)) > 0)
  {
    buf.push_back(charCode);
    wci += charLength;
  }
  buf[charCount] = 0;
  return &buf[0];
}

extern "C" SP_term_ref SPCDECL parseXMLFromFile(char *xmlFile, long flags,
						SP_term_ref messages,
						SP_term_ref dtd,
						SP_atom predName,
						SP_atom module)
{
  SP_term_ref content = 0;
  InitGuard guard;
  try
  {
#ifndef WIN32
    ScopedHandler<std::unexpected_handler,std::set_unexpected>
#else
    ScopedHandler
#endif
      unexp(map_unexpected);
    std::vector<XMLCh> fileName;
    xercesc::LocalFileInputSource is(xerces_encoding(xmlFile, fileName));
    content = parseXML(is, flags, messages, dtd, predName, module);
  }
  catch (PrologException const &e)
  {
    SP_raise_exception(e.getException());
  }
  catch (PrologFailure const &)
  {
    SP_fail();
  }
  catch (xercesc::XMLException const &e)
  {
    raise_exception(prolog_encoding(e.getMessage()));
  }
  catch (std::exception const &e)
  {
    raise_exception(SP_from_os(e.what(), WCX_OPTION));
  }
  catch (...)
  {
    raise_exception("unknown exception");
  }
  return content;
}

// This class supposes that the underlying SICStus stream is
// in binary mode.
class BinSICStusInputStream: public xercesc::BinInputStream
{
public:
  BinSICStusInputStream(SP_stream *stream): stream_(stream) {}
  virtual unsigned int curPos() const;
  virtual unsigned int readBytes(XMLByte *const toFill,
				 const unsigned int maxToRead);

private:
  SP_stream *stream_;
};

unsigned int BinSICStusInputStream::curPos() const
{
  return static_cast<unsigned int>(stream_->byte_count)   ;
}

unsigned int BinSICStusInputStream::readBytes(XMLByte * const toFill,
					      unsigned int const maxToRead)
{
  int b;
  unsigned int i;
  for (i = 0; i < maxToRead && (b = SP_fgetc(stream_)) > -1; ++i)
  {
    toFill[i] = b;
  }
  return i;
}

class SICStusStreamInputSource: public xercesc::InputSource
{
public:
  SICStusStreamInputSource(SP_stream *stream): stream_(stream) {
    if (stream_->filename != 0) {
      std::vector<XMLCh> fileName;
      setSystemId(xerces_encoding(stream_->filename, fileName));
    }
  }
  virtual xercesc::BinInputStream *makeStream() const {
    return new BinSICStusInputStream(stream_);
  }

private:
  SP_stream *stream_;
};

extern "C" SP_term_ref SPCDECL parseXMLFromStream(SP_stream *stream,
						  long flags,
						  SP_term_ref messages,
						  SP_term_ref dtd,
						  SP_atom predName,
						  SP_atom module)
{
  SP_term_ref content = 0;
  InitGuard guard;
  try
  {
#ifndef WIN32
    ScopedHandler<std::unexpected_handler,std::set_unexpected>
#else
    ScopedHandler
#endif
      unexp(map_unexpected);

    SICStusStreamInputSource is(stream);
    content = parseXML(is, flags, messages, dtd, predName, module);
  }
  catch (PrologException const &e)
  {
    SP_raise_exception(e.getException());
  }
  catch (PrologFailure const &)
  {
    SP_fail();
  }
  catch (xercesc::XMLException const &e)
  {
    raise_exception(prolog_encoding(e.getMessage()));
  }
  catch (std::exception const &e)
  {
    raise_exception(SP_from_os(e.what(), WCX_OPTION));
  }
  catch (...)
  {
    raise_exception("unknown exception");
  }
  return content;
}
