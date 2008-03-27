/*
 SenseNet XML reader utility
 Targosoft 2006
 2006
*/

:- module(xml_parser, [parse_XML/1, parse_XML_DOM/2]).
:- use_module(sax, [parseSAX/5, parseDOM/4]). % xerces 2.6 interface

parse_XML(Filename):-
	retractall(sensenet:node(_,_,_)),
	parseSAX(Filename,sax_callback,'',[],
		 [namespace(prefix),whitespce_filter]).

parse_XML_DOM(Filename,OutputXML):-
	parseDOM(Filename,[OutputXML],[],[namespace(prefix),whitespce_filter]).

% ~~~~~~~~~~~~~~~~~~~~~~~
% SAX non-error callbacks
% ~~~~~~~~~~~~~~~~~~~~~~~

sax_callback(start_element(Name,Atts),I,O):-
	start_element_processor(Name,Atts,I,O).
sax_callback(end_element(Name), I, O):-
	end_element_processor(Name, I, O).
sax_callback(characters(Atom),
	     sax(Basename,Gy,[Atom|New],Stack),
	     sax(Basename,Gy,New,Stack)).

% ~~~~~~~~~~
% SAX errors
% ~~~~~~~~~~
sax_callback(warning(F,L,C,M), S, S) :-
	format('XML Parser warning (~w, line ~d, column ~d)~n~w~n', [F,L,C,M]).
sax_callback(error(F,L,C,M), S, S) :-
	format('XML Parser error (~w, line ~d, column ~d)~n~w~n', [F,L,C,M]),
	fail.
sax_callback(fatal_error(F,L,C,M), S, S) :-
	format('XML Parser fatal error (~w, line ~d, column ~d)~n~w~n',
	       [F,L,C,M]),
	fail.

% ~~~~~~~~~~~~
% SAX handlers
% ~~~~~~~~~~~~

% root element can be detected automatically as well
% here we hard code it
start_element_processor(nodes,_Attrs,Base,Output) :-
	!,Output = sax(Base,A,A,[]).
start_element_processor(Name,Atts,
			sax(B,Gy,[Env|EnvT],Stack),
			sax(B,Gy,New,[EnvT|Stack])) :-
        (   Atts == []
	->
	    Env = env(Name, New)
        ;
	    Env = env(Name, Atts, New)
	).

end_element_processor(nodes,_Input,Output):-
	!,Output=[].
end_element_processor(_Name,sax(B,[Gy],[],[_]), sax(B,A,A,[])):-
	% finalizing a first level element
	asserted_data(Gy).

end_element_processor(_Name,sax(B,Gy,[],[Prev|Stack1]), sax(B,Gy,Prev,Stack1)).  

% ~~~~~~~~~~~~~~~
% extracting data
% ~~~~~~~~~~~~~~~

% we assume that attribute id identifies the node
asserted_data(env(node, [id=Count], Children)) :-
	asserted_data(Children, Count).

asserted_data([], _).
asserted_data([env(Name,[Value])|Rest], Count) :-
	assert(sensenet:node(Count, Name, Value)),
	asserted_data(Rest, Count).
	      