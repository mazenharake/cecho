#!/usr/bin/env escript
%%! -noinput -pa ../cecho/_build/default/lib/cecho/ebin +A 50
-include_lib("cecho/include/cecho.hrl").
main(_) -> cecho_example:helloworld().
