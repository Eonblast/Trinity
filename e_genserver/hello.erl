% Eonbeam Dev: Gen Server Skeleton
% --------------------------------
% file: eonbeam/dev/3/d_genserver/hello.erl
% Starts, tests and terminates the hello_gen_server.
% Start with: erl -s hello run -s init stop -noshell

-module(hello).
-export([run/0]).

run() ->

	io:format("hello: starting gen_server~n"),
	{ok, GenServer} = hello_gen_server:start_link(),
	
	io:format("hello: sending hello to gen_server~n"),
	world = gen_server:call(GenServer, hello),
	
	io:format("hello: sending stop to gen_server~n"),
	ok = gen_server:call(GenServer, stop),
	
	io:format("hello: gen_server stopped~n").