
:- use_module(dlog,[start_dlog/0]).
% IMPORTANT: first load the config module, then load the config file, 
%  finally load the rest!

:- use_module('core/config', [target/1,
					get_dlog_option/2, get_dlog_option/3, 
					set_dlog_option/2, set_dlog_option/3,
					load_config_file/0, load_config_file/1]).

%load config at startup
% :- initialization load_config_file.

:- use_module('core/kb_manager' /*, [new_kb/1, release_kb/1]*/).
:- use_module('core/console', [console/0]).
:- use_module('interfaces/dig_iface', [execute_dig_file/2]).
:- use_module('core/dlogger', [error/3, warning/3, info/3, detail/3]).
:- use_module('test/dlog_test', [execute_test_files/2]).

:- meta_predicate dlog_test_swi_tools:time_limit0(0,*,*).
:- meta_predicate tbox_translator:add_generated_predicates(//,?,?).
:- meta_predicate kb_manager:with_read_lock(+,0).
:- meta_predicate kb_manager:with_write_lock(+,0).
:- meta_predicate unfold_main:print_kb(*,*,0,*,*,*).
:- meta_predicate unfold_main:time(*,0).
% Restarting analysis ...
% Found new meta-predicates in iteration 2 (0.094 sec)
:- meta_predicate dlog_test_swi_tools:time_limit(0,*,*).
:- meta_predicate dlog_test_swi_tools:time_limit(0,*).
:- meta_predicate dlog_test:try(0,*,*).
:- meta_predicate dlog_test:try(0,*,*,*).
:- meta_predicate unfold_main:time(0).

% :- use_module('hash/dlog_hash',[init_state/1,new_state/3,new_anc/3,new_loop/3,check_anc/2,check_loop/2]).

:-dlog:start_server.
:-debug.
:-execute_test_files(['../examples/happy/happy.tst'],text) .
:-execute_test_files(['../examples/alcoholic/ca10.tst'],text) .
:-execute_test_files(['../src/test/iocaste.tst'],text) .
% :-execute_test_files(['../src/test/iocasteDB.tst'],text) .

:-execute_test_files(['../src/prolog_translator/zsl_test_subrole01.tst'],text).
:-execute_test_files(['../src/prolog_translator/zsl_test_subrole02.tst'],text).


:-execute_test_files(['../src/prolog_translator/zsl_test01.tst'],text) .
:-execute_test_files(['../src/prolog_translator/zsl_test04.tst'],text) .
:-execute_test_files(['../src/prolog_translator/zsl_test05.tst'],text) .
:-execute_test_files(['../src/prolog_translator/zsl_test06.tst'],text) .
:-execute_test_files(['../src/prolog_translator/zsl_test07.tst'],text) .
:-execute_test_files(['../src/prolog_translator/zsl_test08.tst'],text) .


:-console.


/*

?- make.
% Updating index for library /usr/lib/swi-prolog/library/
Warning: The predicates below are not defined. If these are defined
Warning: at runtime using assert/1, use :- dynamic Name/Arity.
Warning:
Warning: dl_to_fol_old:neq/2, which is referenced by
Warning:        /devel/LogicmooDeveloperFramework/DLog/dlog-server/src/dl_translator/old/dl_to_fol_old.pl:241:12: 8-th clause of dl_to_fol_old:def2/5
Warning: kb_manager:delete_file/2, which is referenced by
Warning:        /devel/LogicmooDeveloperFramework/DLog/dlog-server/src/core/kb_manager.pl:207:40: 3-th clause of kb_manager:add_abox/3
Warning:        /devel/LogicmooDeveloperFramework/DLog/dlog-server/src/core/kb_manager.pl:267:40: 3-th clause of kb_manager:add_tbox/4
Warning: saturate_without_binary_fol:decomposeClause/2, which is referenced by
Warning:        /devel/LogicmooDeveloperFramework/DLog/dlog-server/src/dl_translator/fol/saturate_without_binary_fol.pl:30:19: 3-th clause of saturate_without_binary_fol:saturate_without_binary/3
Warning: saturate_without_binary_old:decomposeClause/2, which is referenced by
Warning:        /devel/LogicmooDeveloperFramework/DLog/dlog-server/src/dl_translator/old/saturate_without_binary_old.pl:29:35: 3-th clause of saturate_without_binary_old:saturate_without_binary/3
Warning: saturate_without_binary_old:elim_reds/3, which is referenced by
Warning:        /devel/LogicmooDeveloperFramework/DLog/dlog-server/src/dl_translator/old/saturate_without_binary_old.pl:35:18: 3-th clause of saturate_without_binary_old:saturate_without_binary/3
Warning: saturate_without_binary_old:redundant/2, which is referenced by
Warning:        /devel/LogicmooDeveloperFramework/DLog/dlog-server/src/dl_translator/old/saturate_without_binary_old.pl:21:33: 2-nd clause of saturate_without_binary_old:saturate_without_binary/3
Warning: saturate_without_binary_old:resolve/3, which is referenced by
Warning:        /devel/LogicmooDeveloperFramework/DLog/dlog-server/src/dl_translator/old/saturate_without_binary_old.pl:28:40: 3-th clause of saturate_without_binary_old:saturate_without_binary/3
Warning: saturate_without_binary_old:simplifyClauses/2, which is referenced by
Warning:        /devel/LogicmooDeveloperFramework/DLog/dlog-server/src/dl_translator/old/saturate_without_binary_old.pl:12:25: 1-st clause of saturate_without_binary_old:saturate_without_binary/2
Warning: unfold_main:functor/2, which is referenced by
Warning:        /devel/LogicmooDeveloperFramework/DLog/dlog-server/src/unfold/ainterp.pl:191:8: 2-nd clause of unfold_main:goal_to_agoal/4
Warning: unfold_main:inc_ancs/1, which is referenced by
Warning:        /devel/LogicmooDeveloperFramework/DLog/dlog-server/src/unfold/unfold.pl:1454:8: 1-st clause of unfold_main:incorrect_ancs/2
Warning: unfold_main:is_called_by/3, which is referenced by
Warning:        /devel/LogicmooDeveloperFramework/DLog/dlog-server/src/unfold/unfold_test.pl:307:8: 2-nd clause of unfold_main:functor_in_prog/2
Warning: unfold_main:xxx/1, which is referenced by
Warning:        /devel/LogicmooDeveloperFramework/DLog/dlog-server/src/unfold/unfold.pl:1368:41: 3-th clause of unfold_main:add_goal1/4
Warning: unfold_main:yyy/0, which is referenced by
Warning:        /devel/LogicmooDeveloperFramework/DLog/dlog-server/src/unfold/unfold.pl:1366:37: 2-nd clause of unfold_main:add_goal1/4
true.

*/
