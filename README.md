# Implement your own Abstract Predicate Streams

18+ Years ago I remember these predicates existed as the building 
blocks for Sockets in some Prolog I cannot remember.


Installation using SWI-Prolog 7.1 or later:

    `?- pack_install(predicate_streams).`

  or

    `?- pack_install('https://github.com/TeamSPoon/predicate_streams.git').`



Source code available and pull requests accepted at http://github.com/TeamSPoon/predicate_streams

# Example usages

```prolog
?- with_output_to_predicate({}/[X]>>assert(saved_output(X)),
     (write("hi there"),nl,writeln("how are you?"))),
     listing(saved_output/1).

saved_output("hi there\n").
saved_output("how are you?\n").
```

```prolog
?- with_input_from_predicate(=('hello.\n'), read(World)).
World = hello.
```

```prolog
% Auto presses Y<Enter>
?- with_input_from_predicate({}/[X]>>X='Y\n', poor_interactive_goal).

```

```prolog

?- with_error_to_predicate(write,threads).
... writes thread info to stdout instead of stderr...

```

```prolog
?- with_output_to_predicate(print_as_html_pre,
    (writeln("hi there"),writeln("how are you?"))).

<pre>hi there
</pre>
<pre>how are you?
</pre>
```

[BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017,

Douglas Miles <logicmoo@gmail.com>

All rights reserved.

