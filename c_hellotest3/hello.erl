% Part of the tutorial to Erlang Common Tests @ eonbeam/dev/3
% This program is the subject of the test, not itself a test.
% Use: erlc hello.erl && erl -pa ./ebin -s hello run -s init stop -noshell

-module(hello).
-export([run/0, run/1, text/1]).

run() -> run("World").

run(T) -> io:format(text(T)).

text(T) -> "Hello " ++ T ++ "!~n".