:- module(predicate_streams,
          [ 
            with_input_from_predicate/2,     % +Pred1, +Goal
            with_output_to_predicate/2,      % +Pred1, +Goal
            with_error_to_predicate/2,       % +Pred1, +Goal

            with_predicate_input_stream/3,   % +Pred1, -Stream, +Goal
            with_predicate_output_stream/3,  % +Pred1, -Stream, +Goal

            is_predicate_stream/1,           % +Stream
            current_predicate_stream/1,      % ?Stream

            set_current_input/1,             % +Stream
            set_current_output/1,            % +Stream
            set_current_error/1,             % +Stream

            new_predicate_output_stream/2,   % +Pred1, -Stream
            new_predicate_input_stream/2,    % +Pred1, -Stream

            current_error/1                  % -Stream
          ]).

/** <module> predicate_streams - Abstract Predicate Streams

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2015
                       
    This program is free software; you can redistribute it and/or
    modify it.

    18+ Years ago I remember these predicates existed as the building 
    blocks for Sockets in some Prolog I cannot remember.

*/

:- meta_predicate
        with_input_from_predicate(:, 0),
        with_output_to_predicate(:, 0),
        with_error_to_predicate(:, 0),

        new_predicate_output_stream(:,-),
        new_predicate_input_stream(:,-),

        whatevah(:),
        with_predicate_output_stream(:, -, 0),
        with_predicate_input_stream(:, -, 0).

% Syntactical commenting
:- meta_predicate(no_op(*)).
no_op(_).

%! current_predicate_stream(?Stream) is nondet.
%
%  Current Streams made by this API
%
:- dynamic(current_predicate_stream/1).
:- volatile(current_predicate_stream/1).

% Hooks 
:- thread_local(stream_write/2).
:- thread_local(stream_close/1).
:- thread_local(stream_read/2).

 	 	 
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



% When $user_input == $current_input
current_input_is_user_input:- stream_property(Stream,alias(user_input)),stream_property(Stream,alias(current_input)).

% When $user_output == $current_output
current_output_is_user_output:- stream_property(Stream,alias(user_output)),stream_property(Stream,alias(current_output)).

% set current input stream and aliases
set_current_input(In):- 
 set_stream(In,  alias(current_input)),
 (current_input_is_user_input -> set_stream(In,  alias(user_input)) ; true),
 set_input(In).

% set current output stream and aliases
set_current_output(Out):- 
 set_stream(Out,  alias(current_output)),
 (current_output_is_user_output -> set_stream(Out,  alias(user_output)) ; true),
 set_output(Out).

% set current error stream and aliases
set_current_error(Err):-
 set_stream(Err, alias(current_error)),
 current_input(In), current_output(Out), 
 set_prolog_IO(In,Out,Err).

% Get current error stream
current_error(Err):-   
  stream_property(Err,alias(current_error))-> true;  % when we set it
  stream_property(Err,alias(user_error)) -> true;
  stream_property(Err,file_no(2)).

% Helps when Ctrl-C is hit while stream is busy
maybe_restore_input(Stream):-
  stream_property(Stream,alias(current_input)),
  \+ stream_property(Stream,alias(user_input)),
  stream_property(Was,alias(user_input)),!,
  set_current_input(Was).
maybe_restore_input(Stream):-
  stream_property(Stream,alias(current_input)),
  stream_property(Stream,alias(user_input)),
  original_input_stream(Was),!,
  set_current_input(Was).
maybe_restore_input(_).   

:- dynamic(original_input_stream/1).

:- ignore((\+ original_input_stream(Was),
   stream_property(Was,alias(user_input)),
   asserta(original_input_stream(Was)))).


%! with_output_to_predicate( :Pred1, :Goal) is nondet.
%
%  Redirects output stream to a predicate
%
%  ===
%  ?- with_output_to_predicate({}/[X]>>assert(saved_output(X)),
%     (write("hi there"),nl,writeln("how are you?"))),
%     listing(saved_output/1).
%
%  saved_output("hi there\n").
%  saved_output("how are you?\n").
%
%  ===

with_output_to_predicate(Pred1,Goal):-
   current_output(Prev),   
   stream_property(Prev,buffer_size(Size)),
   stream_property(Prev,buffer(Type)),
    with_predicate_output_stream(Pred1,Stream,
     (set_stream(Stream, buffer(Type)),
      set_stream(Stream, buffer_size(Size)),
       redo_cleanup_each(
          set_current_output(Stream),
          Goal,
          set_current_output(Prev)))).



%! with_error_to_predicate( :Pred1, :Goal) is nondet.
%
%  Redirects error stream to a predicate
%
%  ===
%  ?- with_error_to_predicate(write,threads).
%  ... writes thread info to stdout instead of stderr...
%  ===

with_error_to_predicate(Pred1,Goal):-
   current_error(Prev),
    with_predicate_output_stream(Pred1,Stream, 
       redo_cleanup_each(
          set_current_error(Stream),
          Goal,
          set_current_error(Prev))).


%! with_input_from_predicate( :Pred1, :Goal) is nondet.
%
%  ===
%  ?- with_input_from_predicate(=('hello.\n'), read(World)).
%  World = hello.
%  ===
%
%  ===
%  Auto presses Y<Enter>
%  ?- with_input_from_predicate({}/[X]>>X='Y\n', poor_interactive_goal).
%  ===

with_input_from_predicate(Pred1,Goal):-
    current_input(Prev),
    setup_call_cleanup(       
       (new_predicate_input_stream(Pred1,Stream),
        set_stream(Stream, buffer_size(1))),
        redo_cleanup_each(set_current_input(Stream),Goal,set_current_input(Prev)),
       (maybe_restore_input(Stream),  % this is a so we dont hit the tracer in Ctrl-C
        whatevah(close(Stream)))).



%! is_predicate_stream(+Stream) is det.
%
%  Checks to see if Stream was made by this API
%
is_predicate_stream(Stream):- 
   must_be(nonvar,Stream),
   current_predicate_stream(Stream).


%! with_predicate_output_stream( :Pred1, ?Stream, :Goal) is nondet.
%
%  Helper that creates and destroys (closes) a predicate output stream
%
with_predicate_output_stream(Pred1,Stream,Goal):-
    setup_call_cleanup(       
       new_predicate_output_stream(Pred1,Stream),
       Goal,
       whatevah(close(Stream))).


%! with_predicate_input_stream( :Pred1, ?Stream, :Goal) is nondet.
%
%  Helper that creates and destroys (closes) a predicate input stream
%
%  used by with_output_to_predicate/2, with_error_to_predicate/2
%
with_predicate_input_stream(Pred1,Stream,Goal):-
    setup_call_cleanup(
       new_predicate_output_stream(Pred1,Stream),
       Goal,
       whatevah(close(Stream))). 


% =====================================================
% All magic is below here
% =====================================================

:- use_module(library(prolog_stream)).

%! new_predicate_output_stream(:Pred1,-Stream)
%
%  Creates a new output stream that each write 
%  Invokes: call(+Pred1,+Data).

new_predicate_output_stream(Pred1,Stream):-
  open_prolog_stream(predicate_streams, write, Stream, []),
  asserta((stream_write(Stream,Data):- whatevah(call(Pred1,Data))),Ref1),
  asserta(current_predicate_stream(Stream),Ref2),
  asserta((
    stream_close(Stream):- 
    debug(predicate_streams,'~N% ~q.~n',[(stream_close(Stream):-flusing_output_to(Pred1))]),
       whatevah(flush_output(Stream)),
       whatevah(erase(Ref1)),
       whatevah(erase(Ref2)),
       retractall(stream_close(Stream)))).

%! new_predicate_input_stream(:Pred1,-Stream)
%
%  Creates a new input stream that each read/getch()
%  Invokes: call(+Pred1,-Data).
%  
% @todo Discuss how to handle peek_char/2

new_predicate_input_stream(Pred1,Stream):-
  open_prolog_stream(predicate_streams, read, Stream, []),
  asserta((stream_read(Stream,Data):- ignore(whatevah(call(Pred1,Data)))),Ref1),
  asserta(current_predicate_stream(Stream),Ref2),
  asserta((
    stream_close(Stream):-    
    debug(predicate_streams,'~N% ~q.~n',[(stream_close(Stream):-call(Pred1,end_of_file))]),
       whatevah(erase(Ref1)),
       whatevah(erase(Ref2)),       
       retractall(stream_close(Stream)))).


%! whatevah( :Goal) is semidet.
%
% As pronounced by a teenage girl
%
whatevah(Goal):- ignore(catch(Goal,error(_,_),fail)).

% set_stream(Stream, buffer(line)), % useful?
% set_stream(Stream, buffer_size(1)),   % useful? 
% set_stream(Stream, close_on_exec(true)), % useful?
% set_stream(Stream, close_on_abort(true)), % useful?

