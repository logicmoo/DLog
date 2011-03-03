:- use_module('zsl_util').
%:- use_module('query_to_sql_helpers/mute_debug.pl').
%:- load_files('query_to_sql_helpers/mute_debug.pl').

%debugflag deb1
deb1(yes).

test1:- writecdeb('muetedebug helloW',deb1).

test2:- open('g:\\code\\dlog2\\dlog-server\\src\\prolog_translator\\mutedebtest.txt',write,Handle),
   writecdeb(Handle,'mutedeb test',deb1),
   close(Handle).