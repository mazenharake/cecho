#!/usr/bin/env escript
%%! -noinput -pa ./ebin -pa _build/default/lib/cecho/ebin +A 10
-include_lib("include/cecho.hrl").
main(_) -> cecho_example:countdown().
