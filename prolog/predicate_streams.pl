% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/predicate_streams.pl

:- module(predicate_streams,
          [ 
            with_input_from_pred/2,
            with_output_to_pred/2,
            with_error_to_pred/2,

            with_predicate_input_stream/3,
            with_predicate_output_stream/3,
            is_predicate_stream/1,

            set_current_input/1,
            set_current_output/1,
            set_current_error/1,

            current_error/1
          ]).

:- meta_predicate
        with_error_to_pred(:, 0),
        with_input_from_pred(:, 0),
        with_output_to_pred(:, 0),
        whatever(0),
        with_predicate_output_stream(:, -, 0),
        with_predicate_input_stream(:, -, 0).


 	 	 
:- meta_predicate(redo_cleanup_each(0,0,0)).
redo_cleanup_each(Setup,Goal,Cleanup):-
   \+ \+ '$sig_atomic'(Setup), 
   catch( 
     ((Goal, deterministic(DET)),
       '$sig_atomic'(Cleanup),
         (DET == true -> !
          ; (true;('$sig_atomic'(Setup),fail)))), 
      E, 
      ('$sig_atomic'(Cleanup),throw(E))). 


plz_set_stream(S,P):- whatever(set_stream(S,P)).

set_current_input(In):- 
 plz_set_stream(In,  alias(current_input)),
 plz_set_stream(In,  alias(user_input)),
 set_input(Stream).

set_current_output(Out):- 
 plz_set_stream(Out,  alias(current_output)),
 plz_set_stream(Out,  alias(user_output)),
 set_output(Out).

set_current_error(Err):- current_input(In), current_output(Out), 
 set_prolog_IO(In,Out,Err),
 set_stream(Err, alias(current_error)),
 set_stream(Err, alias(user_error)).


current_error(Err):-   
  stream_property(Err,alias(current_error))-> true;  % when we set it
  stream_property(Err,alias(user_error)) -> true;
  stream_property(Err,file_no(2)).



%! whatever( :Goal) is semidet.
%
% Pronounced like a teenage girl
%
whatever(Goal):- ignore(catch(Goal,error(_,_),fail)).


%! with_output_to_pred( :Pred1, :Goal) is nondet.
%
%  Redirects output stream to a predicate
%
%  `?- with_output_to_pred(print_as_html_pre,
%    (writeln("hi there"),writeln("how are you?"))).`
%
% @bug Not decided that a with_output_to_pred/2 should call close or not?
% (flush gets the job done equally as well as closing)

with_output_to_pred(Pred1,Goal):-
    current_output(Prev),
    with_predicate_output_stream(Pred1,Stream,
        redo_cleanup_each(set_current_output(Stream),Goal,set_current_output(Prev))).


%! with_error_to_pred( :Pred1, :Goal) is nondet.
%
%  Redirects error stream to a predicate
%
% `?- with_error_to_pred(write,threads).`
%
% @bug Not decided that a with_error_to_pred/2 should call close or not?
% (flush gets the job done equally as well as closing)

with_error_to_pred(Pred1,Goal):-
    current_error(Prev),
    with_predicate_output_stream(Pred1,Stream,
        redo_cleanup_each(set_current_error(Stream),Goal,set_current_error(Prev))).



%! with_input_from_pred( :Pred1, :Goal) is nondet.
%
%  `?- with_input_from_pred((^(X):-X = 'y\n'), poor_interactive_goal).`
%
with_input_from_pred(Pred1, Goal):-
    current_input(Prev),
    with_predicate_input_stream(Pred1,Stream,
        redo_cleanup_each(set_current_input(Stream),Goal,set_current_input(Prev))).


is_predicate_stream(Stream):-
   transient_pred_stream:is_pred_stream(Stream).

:- multifile(transient_pred_stream:is_pred_stream/1).
:- dynamic(transient_pred_stream:is_pred_stream/1).
:- volatile(transient_pred_stream:is_pred_stream/1).
:- export(transient_pred_stream:is_pred_stream/1).



% =====================================================
% All the magic is below here
% =====================================================

:- use_module(library(prolog_stream)).

:- thread_local(tl_pred_streams:stream_write/2).
:- module_transparent(tl_pred_streams:stream_write/2).
:- thread_local(tl_pred_streams:stream_close/1).
:- module_transparent(tl_pred_streams:stream_close/1).

with_predicate_output_stream(Pred1,Stream,Goal):- 
  open_prolog_stream(tl_pred_streams, write, Stream, []),  
  % plz_set_stream(Stream, buffer(line)),  <- Segfaultz
  % plz_set_stream(Stream, buffer(false)), % usefull?
  % plz_set_stream(Stream, buffer_size(0)),   % usefull? 
  % plz_set_stream(Stream, close_on_exec(false)), % usefull?
  % plz_set_stream(Stream, close_on_abort(false)), % usefull?
   setup_call_cleanup(
   ( asserta((tl_pred_streams:stream_write(Stream,Data):- ignore(predicate_streams:whatever(call(Pred1,Data)))),Ref1),
     asserta((tl_pred_streams:stream_close(Stream):- ignore(predicate_streams:whatever(call(Pred1,end_of_file)))),Ref2),
     asserta(transient_pred_stream:is_pred_stream(Stream),Ref3)),
    Goal,
    % catch so we will not exception on a closed stream
   (predicate_streams:whatever(flush_output(Stream)),erase(Ref1),erase(Ref2),erase(Ref3))).
  

:- thread_local(tl_pred_streams:stream_read/2).
:- module_transparent(tl_pred_streams:stream_read/2).

with_predicate_input_stream(Pred1,Stream,Goal):- 
  open_prolog_stream(tl_pred_streams, read, Stream, []),
   setup_call_cleanup(
   ( asserta((tl_pred_streams:stream_read(Stream,Data):- ignore(predicate_streams:whatever(call(Pred1,Data)))),Ref1),
     asserta(transient_pred_stream:is_pred_stream(Stream),Ref2)),
    Goal,
   (erase(Ref1),erase(Ref2))).




