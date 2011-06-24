% Eonbeam Dev: Gen Server Skeleton
% --------------------------------
% file: eonbeam/dev/3/g_supervisor/hello.erl
% Starts, tests and terminates the hello_supervisor and by that the hello_gen_server.
% Start with: erl -s hello run -s init stop -noshell

-module(hello).
-export([run/0]).

run() ->

	% kind of magicians gesture: check that the gen server is not (yet) running
	undefined = whereis(hello_gen_server),

	io:format("hello: starting supervisor~n"),
	{ok, _} = hello_supervisor:start_link(),
	
	% ------------------------------------------------
	io:format("hello: sending hello to gen_server~n"),
	world = gen_server:call(hello_gen_server, hello),
	% ------------------------------------------------
	
	io:format("hello: sending stop to gen_server~n"),
	ok = gen_server:call(hello_gen_server, stop),
	
	io:format("hello: gen_server stopped (but the supervisor should restart it)~n"),
	
	receive after 100 -> nil end, % let restart

	% check that the gen server is in fact running again
	true = erlang:is_process_alive(whereis(hello_gen_server)),
	
	io:format("hello: sending hello to gen_server~n"),
	world = gen_server:call(hello_gen_server, hello),
	
	io:format("hello: sending stop to gen_server~n"),
	ok = gen_server:call(hello_gen_server, stop),
	
	io:format("hello: gen_server stopped (and now the supervisor should NOT restart it)~n"),
	% because of the strategy set in hello_supervisor.erl
	
	receive after 100 -> nil end,  % let restart (not, of course)
	
	% check that the gen server process is really gone.
	undefined = whereis(hello_gen_server),

	io:format("hello: bingo~n"), % else would have crashed before
	
	ok.	