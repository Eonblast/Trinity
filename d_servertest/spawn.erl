% for test #2, spawning client and server from one Erlang process.
% Start with: erl -s spawn start -s init stop -noshell

% Try: 1) ./1_pathetic_test.sh, 2) ./2_spawn_test.sh, 3) ./3_ct_test.sh

-module(spawn).
-export([start/0]).

start() ->

	io:format("starting spawner.~n"),

	spawn(server, start, []),
	spawn(client, start, []),

	io:format("spawner done.~n").
