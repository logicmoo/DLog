 :- use_module(dlog,[start_dlog/0]).

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


 :-start_dlog.


