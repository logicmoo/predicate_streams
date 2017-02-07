% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/predicate_streams.pl

:- module(predicate_streams,
          [ with_err_to_pred/2,
            with_input_from_pred/2,
            with_output_to_pred/2,
            with_write_stream_pred/4,
            set_error_stream/1,
            set_output/1,
            with_io_restore/1,
            current_error/1
          ]).
:- meta_predicate
        with_err_to_pred(:, 0),
        with_input_from_pred(:, 0),
        with_output_to_pred(:, 0),
        with_io_restore(0),
        on_x_fail_priv(0),
        with_write_stream_pred(:, -, 0, 0).


%  ?- with_err_to_pred(write,format(user_error,'~s',["ls"])).


plz_set_stream(S,P):- ignore(predicate_streams:on_x_fail_priv(set_stream(S,P))).


%! on_x_fail_priv( :Goal) is semidet.
%
% If there If Is A an exception in  :Goal goal then fail.
%
on_x_fail_priv(Goal):- catch(Goal,_,fail).

:- use_module(library(prolog_stream)).

:- multifile(transient_plstream:is_prolog_stream/1).
:- dynamic(transient_plstream:is_prolog_stream/1).
:- volatile(transient_plstream:is_prolog_stream/1).
:- export(transient_plstream:is_prolog_stream/1).


:- thread_local(tl_with_prolog_streams:stream_close/1).
:- meta_predicate(tl_with_prolog_streams:stream_close(-)).


:- thread_local(tl_with_prolog_streams:stream_write/2).
:- meta_predicate(tl_with_prolog_streams:stream_write(?,?)).

:- meta_predicate(with_output_to_pred(1,0)).


%! with_io_restore(Goal) is semidet.
%
% Using Input/output.
%
with_io_restore(Goal):-
  current_input(IN),current_output(OUT),current_error(Err),  
  setup_call_cleanup(set_prolog_IO(IN,OUT,Err),
    call(Goal), 
    set_prolog_IO(IN,OUT,Err)).
 

set_main_io:- (thread_self(main)->((stream_property(Err, alias(user_error)),set_stream(Err,alias(user_error))),
       (stream_property(In, alias(user_input)),set_stream(In,alias(user_input))),
       (stream_property(Out, alias(user_output)),set_stream(Out,alias(user_output))),
                                    set_stream(Err,alias(current_error)));true).


current_error(Err):- current_error0(Err),
  (thread_self(main)->true; \+ stream_property(Err,alias(user_error))), 
  !. % stream_property(Err,type(text)),!.

current_error0(Err):- clause(current_error1(Err),B),catch(B,_,fail).
current_error1(Err):- stream_property(Err,alias(current_error)). % for if/when we set it
current_error1(Err):- stream_property(Err,alias(user_error)).
current_error1(Err):- stream_property(user_error,file_no(N)),quintus:current_stream(N,write,Err).
current_error1(Err):- current_output(Out),quintus:current_stream(X,write,Out),integer(X),Y is X+1,quintus:current_stream(Y,write,Err), \+ stream_property(Err,file_name(_)), \+ stream_property(Err,alias(_)).
current_error1(Err):- current_output(Out),stream_property(Err,output),Err\==Out, \+ stream_property(Err,file_name(_)), \+ stream_property(Err,alias(_)).
% current_error1(Err):- get_thread_current_error(Err).
current_error1(Err):- stream_property(Err,alias(current_error)).

% current_output(Out),quintus:current_stream(X,write,Out),Y is X+1,quintus:current_stream(Y,write,Err),stream_property(Err,Prop).
 	 	 

%! with_output_to_pred( :PRED1Callback, :Goal) is semidet.
%
% Using Output Converted To Predicate.
%
with_output_to_pred(Callback,Goal):-
  current_output(Prev), FinalClean = set_output(Prev),
    with_write_stream_pred(Callback,Out,
      (set_output(Out),Goal,FinalClean),FinalClean).


% if it''s not aliased already maybe we could set current_error alias?
set_error_stream(Err):- current_input(In), 
    current_output(Out), set_prolog_IO(In,Out,Err).


%! with_err_to_pred( :PRED1Callback, :Goal) is semidet.
%
% Using Err Converted To Predicate.
%
with_err_to_pred(Callback,Goal):-
    current_error(Prev),
    with_write_stream_pred(Callback,Stream,
        (set_error_stream(Stream),Goal),
          set_error_stream(Prev)).
 	 	 

%! with_write_stream_pred( :PRED1Callback, -Stream, :Goal, :GoalExit) is semidet.
%
% Using Output Converted To Stream Predicate.
%
% % Not decided that a with_output_to_pred/2 should call close or not (flush gets the job done equally as well as closing)

with_write_stream_pred(Callback,Stream,Goal,Exit):- 
  open_prolog_stream(tl_with_prolog_streams, write, Stream, []),
  % catch so we will not exception on a closed stream
  % plz_set_stream(Stream, buffer(line)),
  % plz_set_stream(Stream, buffer(false)),
  % plz_set_stream(Stream, buffer_size(0)),    
  % plz_set_stream(Stream, close_on_exec(false)),
  % plz_set_stream(Stream, close_on_abort(false)),

  call_cleanup(
   really_need_setup_call_cleanup_each(
   ( asserta(((tl_with_prolog_streams:stream_write(Stream,Data):- (ignore(predicate_streams:on_x_fail_priv(call(Callback,Data)))))),Ref),
     asserta(((tl_with_prolog_streams:stream_close(Stream):- (ignore(predicate_streams:on_x_fail_priv(call(Callback,end_of_file)))))),Ref2),
     call(asserta,transient_plstream:is_prolog_stream(Stream),Ref3)),
    Goal,
   (catch(flush_output(Stream),_,true),erase(Ref),erase(Ref2),erase(Ref3))),
  Exit).

  

:- thread_local(tl_with_prolog_streams:stream_read/2).
:- meta_predicate(tl_with_prolog_streams:stream_read(?,?)).
:- meta_predicate(prolog_stream:open_prolog_stream(?,?,?,?)).

 	 	 
:- meta_predicate(really_need_setup_call_cleanup_each(0,0,0)).
really_need_setup_call_cleanup_each(S,G,C):-setup_call_cleanup(S,G,C).


%! with_input_from_pred( :PRED1Callback, :Goal) is semidet.
%
% Using Input Converted From Predicate.
%
with_input_from_pred(Callback,Goal):-
 current_input(Old),
 really_need_setup_call_cleanup_each(true,
 ((
   must_be(var,Stream),
    tl_with_prolog_streams:open_prolog_stream(tl_with_prolog_streams, read, StreamO, []),
    StreamO = Stream,
   asserta(((tl_with_prolog_streams:stream_read(Stream,Data):- ((call(Callback,Data))))),Ref),
   asserta(((tl_with_prolog_streams:stream_close(Stream):- (ignore(call(Callback,end_of_file))))),Ref2),
   plz_set_stream(Stream,  alias(current_input)),
   plz_set_stream(Stream,  alias(user_input)),
   setup_call_cleanup(see(Stream), Goal,(seen,erase(Ref2),erase(Ref))))),
 set_input(Old)).




