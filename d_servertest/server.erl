% Minimal one-off server: receives one message at localhost:1234 and prints it.
% See: Erlang Programming, pg 329
% Start into background with: erl -s server start -s init stop -noshell -noinput &
% Try: 1) ./1_pathetic_test.sh, 2) ./2_spawn_test.sh, 3) ./3_ct_test.sh

-module(server).
-export([start/0, start/1]).

start() ->

	start(1234).
	
start(Port) ->

	io:format("server started.~n"),
	{ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
	wait_connect(ListenSocket),
	io:format("server done.~n"),
	ok.	

wait_connect(ListenSocket) ->

	{ok, Socket} = gen_tcp:accept(ListenSocket),
	case gen_tcp:recv(Socket, 0, 5000) of
		{ok, Binary} ->
			io:format("server got: ~p~n", [Binary]);
		{error, closed} ->
			io:format("server finds socket closed in get_request~n")
	end.

