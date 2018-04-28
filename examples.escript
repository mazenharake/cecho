#!/usr/bin/env escript
%%! -noinput -pa ../cecho/_build/default/lib/cecho/ebin +A 50

%% Read the LICENSE file
-include_lib("cecho/include/cecho.hrl").
main(["countdown"]) ->
  cecho_example:countdown();
main(["simple"]) ->
  cecho_example:simple();
main(["colors"]) ->
  cecho_example:colors();
main(["input"]) ->
  cecho_example:input();
main(["cursmove"]) ->
  cecho_example:cursmove();
main(["helloworld"]) ->
  cecho_example:helloworld();
main(_) ->
  io:format("Usage: ./examples.escript <example>\n"),
  io:format("\n"),
  io:format("where <example> is one of\n"),
  io:format("\n"),
  io:format("    countdown - Simple countdown which shows\n"),
  io:format("                how to print, move and get\n"),
  io:format("                coordinates\n"),
  io:format("\n"),
  io:format("    simple - Simple example to show usage\n"),
  io:format("\n"),
  io:format("    colors - Fun with colors\n"),
  io:format("\n"),
  io:format("    input - Prints a number continuously as another\n"),
  io:format("            io thread is waiting for keyinput\n"),
  io:format("\n"),
  io:format("    cursmove - move the '@' around the screen with the\n"),
  io:format("               arrow keys. 'q' to quit.\n"),
  io:format("\n"),
  io:format("    helloworld - bounce 'Hello World!' around the screen\n").
