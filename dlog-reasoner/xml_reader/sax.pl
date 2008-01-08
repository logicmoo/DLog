:- module(sax, [parseSAX/5,parseDOM/4]).

:- use_module(library(lists), [reverse/2]).

:- meta_predicate parseSAX(+,:,+,-,+).
parseSAX(FileName, Callback, StartState, EndState, Options) :-
	ErrGoal = parseSAX(FileName,Callback,StartState,EndState,Options),
	must_be_callback(Callback, ErrGoal, Module, Pred, 0, F0),
	parseXML(FileName, EndState, StartState, Options, Module, Pred,
		 F0, ErrGoal).

parseDOM(FileName, Contents, Messages, Options) :-
	ErrGoal = parseDOM(FileName,Contents,Messages,Options),
	parseXML(FileName, Contents, Messages0, Options, [], [], 0, ErrGoal),
	Messages = Messages0.

parseXML(FileName, EndState, StartState, Options, Module, Pred, F0, ErrGoal) :-
	absolute_file_name(FileName, AbsFileName, [access(read)]),
	process_options(Options, DTD, ErrGoal, F0, SAXFlags),
	c_parseXML(AbsFileName, SAXFlags, EndState, StartState, RDTD,
		   Pred, Module),
	normal_dtd(RDTD, [], DTD).
	
process_options([], _DTD, _ErrGoal) --> [].
process_options([O|Os], DTD, ErrGoal) -->
	process_option(O, DTD, ErrGoal),
	process_options(Os, DTD, ErrGoal).

process_option(validation(Validation), _, ErrGoal) --> !,
	must_be_validation(Validation, ErrGoal).
process_option(namespace(Namespace), _, ErrGoal) --> !,
	must_be_namespace(Namespace, ErrGoal).
process_option(schema(Schema), _, ErrGoal) --> !,
	must_be_schema(Schema, ErrGoal).
process_option(dtd(DTD), DTD0, ErrGoal) --> !,
	{   var(DTD), DTD = DTD0
	->  true
	;   option_illarg(domain(var, 'variable'), ErrGoal, DTD)
	},
	dtd_flag.
process_option(whitespce_filter, _, _) --> !, whitespce_filter_flag.
process_option(no_env_compression, _, _) --> !, no_env_compression_flag.
process_option(list_content, _, _) --> !, list_content_flag.
process_option(Option, _, ErrGoal) -->
	{option_illarg(domain(term, 'SAX option'), ErrGoal, Option)}.

must_be_validation(Validation, ErrGoal, _, _) :-
	var(Validation), !,
	option_illarg(var, ErrGoal, Validation).
must_be_validation(auto, _, F0, F) :- !, F is F0 \/ 3.
must_be_validation(always, _, F0, F) :- !, F is F0 \/ 1.
must_be_validation(never, _, F0, F) :- !, F = F0.
must_be_validation(Validation, ErrGoal, _, _) :-
	option_illarg(domain(atom, one_of([auto,always,never])),
		      ErrGoal, Validation).

must_be_namespace(Namespace, ErrGoal, _, _) :-
	var(Namespace), !,
	option_illarg(var, ErrGoal, Namespace).
must_be_namespace(on, _, F0, F) :- !, F is F0 \/ 4.
must_be_namespace(prefix, _, F0, F) :- !, F is F0 \/ 8.
must_be_namespace(expansion, _, F0, F) :- !, F is F0 \/ 20.
must_be_namespace(inheritance, _, F0, F) :- !, F is F0 \/ 1028.
must_be_namespace(Namespace, ErrGoal, _, _) :-
	option_illarg(domain(atom, one_of([on,prefix,expansion,inheritance])),
		      ErrGoal, Namespace).

must_be_schema(Schema, ErrGoal, _, _) :-
	var(Schema), !,
	option_illarg(var, ErrGoal, Schema).
must_be_schema(plain, _, F0, F) :- !, F is F0 \/ 32.
must_be_schema(full, _, F0, F) :- !, F is F0 \/ 64.
must_be_schema(Schema, ErrGoal, _, _) :-
	option_illarg(domain(atom, one_of([plain,full])),
		      ErrGoal, Schema).

must_be_callback(MPred, _, Module, Pred, F0, F) :-
	callback_module(MPred, Module0, Pred0), !,
	Module = Module0,
	Pred = Pred0,
	F is F0 \/ 2048.
must_be_callback(MPred, ErrGoal, _, _, _, _) :-
	option_illarg(domain(term, 'SAX callback'), ErrGoal, MPred).

callback_module(MPred, Module, Pred) :- % this will do for the moment
	nonvar(MPred),
	MPred = Module0:Pred0,
	(   Pred0 = _:_
	->  callback_module(Pred0, Module, Pred)
	;   Module = Module0,
	    Pred = Pred0,
	    atom(Module),
	    atom(Pred),
	    current_predicate(Module:Pred/3)
	).

dtd_flag(F0, F) :- F is F0 \/ 128.

whitespce_filter_flag(F0, F) :- F is F0 \/ 256.

no_env_compression_flag(F0, F) :- F is F0 \/ 512.

list_content_flag(F0, F) :- F is F0 \/ 4096.

normal_dtd([], DTD, DTD).
normal_dtd([RE|RDTD], DTD0, DTD) :-
	normal_attr(RE, E),
	normal_dtd(RDTD, [E|DTD0], DTD).

normal_attr(entity(Name,RAttrs,Model0), entity(Name,Attrs,Model)) :-
	simplified_singleton_pcdata(Model0, Model),
	reverse(RAttrs, Attrs).
normal_attr(entity(Name,Model0), entity(Name,Model)) :-
	simplified_singleton_pcdata(Model0, Model).

simplified_singleton_pcdata(*('#PCDATA'), PCDATA) :- !,
	PCDATA = '#PCDATA'.
simplified_singleton_pcdata(Model, Model).

option_illarg(Error, ErrGoal, Culprit) :-
	functor(ErrGoal, _, Pos),
	prolog:illarg(Error, ErrGoal, Pos, Culprit).

foreign(parseXML, c_parseXML(+string,+integer,[-term],+term,-term,
			     +atom,+atom)).

foreign_resource(sax, [parseXML]).

:- load_foreign_resource(sax).
