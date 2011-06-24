% Eonbeam Dev: Supervisor and Gen Server Skeleton
% -----------------------------------------------
% file: eonbeam/dev/3/i_supervisortest2/hello.erl
% This file is just a preliminary test, not really important.
% Starts, tests and terminates the hello_gen_server directly, without supervisor.
% Then uses the supervisor to start and restart the genserver, checking responses.
% Start with: erl -s hello run -s init stop -noshell

-module(hello).
-export([run/0]).

run() ->

	%% DIRECT PART: (identical to h) ----------------------------------

	io:format("hello: starting gen_server (directly, stand alone)~n"),
	{ok, GenServer} = hello_gen_server:start_link(),
	
	io:format("hello: sending hello to gen_server~n"),
	world = gen_server:call(GenServer, hello),
	
	io:format("hello: sending stop to gen_server~n"),
	ok = gen_server:call(GenServer, stop),
	
	io:format("hello: gen_server stopped~n"),

	receive after 100 -> nil end, % let stop

	%% SUPERVISOR PART: -----------------------------------------------
	io:format("- - -~n"),
	
	% kind of magicians gesture: check that the gen server is not running anymore
	undefined = whereis(hello_gen_server),

	io:format("hello: starting supervisor (will NOT start gen server) **~n"),
	{ok, SuperPid} = hello_supervisor:start_link(),
	
	% ** different from h **
	io:format("hello: add dynamic gen server child to the supervisor **~n"),
	{ok, _} = supervisor:start_child(SuperPid,      
											   % child specs:
 		{hello_gen_server,                     % child id (a special kind of id!)
 		  {hello_gen_server, start_link, []},  % MFA
 		  permanent,                           % kind (permantent = always restart)
 		  1,                                   % max time to shutdown
 		  worker,                              % type
 		  [hello_gen_server]                   % used modules (for code changes)
 		 }),
	
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