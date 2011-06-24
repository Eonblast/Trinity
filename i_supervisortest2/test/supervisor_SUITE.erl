%%%-------------------------------------------------------------------
%%% File     : eonbeam/dev/3/i_supervisortest2/test/genserver_SUITE.erl
%%% Descr    : complete skeleton of a Common Test test suite 
%%% Author   : H. Diedrich
%%% Source   : Erlang docs 'small suite sample'
%%%            www.erlang.org/doc/apps/common_test/example_chapter.html
%%% Adapted  : 06/22/2011 hd
%%%-------------------------------------------------------------------
%%%
%%% Run: eonbeam/dev/3/i_supervisortest2/2_common_test.sh
%%% Or : ct_run -pa . -dir test (from eonbeam/dev/3/i_supervisortest2)
%%%
%%%-------------------------------------------------------------------

-module(supervisor_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%% Mandatory list of test cases and test groups, and skip orders. 

all() -> 
    [white_box_tests, black_box_tests].


%% The test cases. The return value is irrelevant. What matters is, if they return.

%%% Unit tests of gen server call backs ('white box')
%%% White box testing grapples directly with the individual SEQUENTIAL callback 
%%% functions, which is not that exciting for a supervisor. By definition there
%%% should be at best no functionality in a supervisor, so nothing to test.
%%% We do test the expected child spec, maybe out of place but could come handy.

white_box_tests(_Config) -> 

	ct:log(sys_state, "white_box_tests: start~n"),

	% check start_link/0

	ct:log(sys_state, "white_box_tests: start supervisor (this one has no static child, but never mind) **~n"),
	{ok, SuperPid} = hello_supervisor:start_link(),

	% clean up the instance we just started (as a 'side effect')
	ct:log(sys_state, "white_box_tests: stopping the supervisor~n"),
	exit(SuperPid, normal),


	receive after 200 -> nil end, % let stop

	% check init/1

	ExpectedSupervisorSpec = 
    	{one_for_one, 1, 60},                  % restart strategy, times, time range
 	
	ExpectedChildSpec = 
 		[{hello_gen_server,                    % child id (a special kind of id!)
 		  {hello_gen_server, start_link, []},  % MFA
 		  permanent,                           % kind (permantent = always restart)
 		  1,                                   % max time to shutdown
 		  worker,                              % type
 		  [hello_gen_server]                   % used modules (for code changes)
 		 }],
 	
	ct:log(sys_state, "white_box_tests: call supervisor init directly (may often make no sense)~n"),
 	{ok, {ExpectedSupervisorSpec, []}} = % no child spec **
 		hello_supervisor:init([]),
	
	% well, that's it for white boxing. Rather trivial. But this is nice:
	
	% check child spec syntax (** actually, not used yet at this point)
	
	ct:log(sys_state, "white_box_tests: check expected child specs~n"),
	ok = supervisor:check_childspecs(ExpectedChildSpec),
	
	ok.

%%% Functional test of the gen server calls from outside ('black box')
%%% This actually starts the server and then communicates with it.
	
black_box_tests(_Config) -> 

	ct:log(sys_state, "black_box_tests: start~n"),

	%% "SUPERVISOR PART": ---------------------------------------------

	process_flag(trap_exit, true),

	% check that the gen server is not running (yet)
	undefined = whereis(hello_gen_server),

	ct:log(sys_state, "black_box_tests: starting supervisor (will NOT start gen server) **~n"),
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
	ct:log(sys_state, "black_box_tests: sending hello to gen_server~n"),
	world = gen_server:call(hello_gen_server, hello),
	% ------------------------------------------------
	
	ct:log(sys_state, "black_box_tests: sending stop to gen_server~n"),
	ok = gen_server:call(hello_gen_server, stop),
	
	ct:log(sys_state, "black_box_tests: gen_server stopped (but the supervisor should restart it)~n"),
	
	receive after 100 -> nil end, % let restart

	% check that the gen server is in fact running again
	true = erlang:is_process_alive(whereis(hello_gen_server)),
	
	ct:log(sys_state, "black_box_tests: sending hello to gen_server~n"),
	world = gen_server:call(hello_gen_server, hello),
	
	ct:log(sys_state, "black_box_tests: sending stop to gen_server~n"),
	ok = gen_server:call(hello_gen_server, stop),
	
	ct:log(sys_state, "black_box_tests: gen_server stopped (and now the supervisor should NOT restart it)~n"),
	% because of the strategy set in hello_supervisor.erl
	
	receive 
		{'EXIT',_,Reason} -> 
			ct:log(sys_state, "black_box_tests: gen_server EXIT: ~p~n", [Reason])
		after 1000 -> nil % let try to restart (should not, of course)
	end,  
	
	% check that the gen server process is really gone.
	undefined = whereis(hello_gen_server),
	undefined = whereis(hello_supervisor),

	ct:log(sys_state, "black_box_tests: bingo~n"), % else would have crashed before
	
	ok.		
	
