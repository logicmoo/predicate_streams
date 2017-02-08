# Implement your own abstract predicate streams


Installation using SWI-Prolog 7.1 or later:

    `?- pack_install(predicate_streams).`

  or

    `?- pack_install('https://github.com/TeamSPoon/predicate_streams.git').`


Source code available and pull requests accepted at http://github.com/TeamSPoon/predicate_streams

# Example usages

```prolog
?- with_output_to_pred(print_as_html_pre,
    (writeln("hi there"),writeln("how are you?"))).

<pre>hi there
</pre>
<pre>how are you?
</pre>
```

```prolog

?- with_input_from_pred((^(X):-X = 'y\n'), poor_interactive_goal).

```

```prolog

?- with_error_to_pred(write,threads).

```

[BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017,

Douglas Miles <logicmoo@gmail.com>

All rights reserved.

