# Implement your own prolog streams with predicate sources and sinks


Installation using SWI-Prolog 7.1 or later:

    `?- pack_install(predicate_streams).`

  or

    `?- pack_install('https://github.com/TeamSPoon/predicate_streams.git').`

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/TeamSPoon/predicate_streams

```prolog

?- with_output_to_pred(print_as_html_pre,
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

