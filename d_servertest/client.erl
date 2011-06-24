% Minimal one-off TCP client, sending fixed content on fixed localhost  port
% Start with: erl -s client start -s init stop -noshell
% (Start the server first, see server.erl)

% Try: 1) ./1_pathetic_test.sh, 2) ./2_spawn_test.sh, 3) ./3_ct_test.sh

-module(client).
-export([start/0]).

start() ->

	{ok, Socket} = gen_tcp:connect(localhost, 1234, [binary, {packet, 0}]),
	gen_tcp:send(Socket, "+++ This is some stuff sent from the client! +++"),
	ok = gen_tcp:close(Socket),
	ok.
