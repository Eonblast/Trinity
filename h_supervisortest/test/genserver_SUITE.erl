%%%-------------------------------------------------------------------
%%% File     : eonbeam/dev/3/h_supervisortest/test/genserver_SUITE.erl
%%% Descr    : complete skeleton of a Common Test test suite 
%%% Author   : H. Diedrich
%%% Source   : Erlang docs 'small suite sample'
%%%            www.erlang.org/doc/apps/common_test/example_chapter.html
%%% Adapted  : 06/22/2011 hd
%%%-------------------------------------------------------------------
%%%
%%% Run: eonbeam/dev/3/h_supervisortest/2_common_test.sh
%%% Or : ct_run -pa . -dir test (from eonbeam/dev/3/h_supervisortest)
%%%
%%%-------------------------------------------------------------------

-module(genserver_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%% Mandatory list of test cases and test groups, and skip orders. 

all() -> 
    [white_box_tests, black_box_tests].


%% The test cases. The return value are irrelevant. What matters is, if they returns.

%%% Unit tests of gen server call backs ('white box')
%%% This grapples directly with the individual SEQUENTIAL callback functions
%%% that implement the specifics of the concrete gen server at hand. It does
%%% NOT test the gen server as an entity but its parts, in unit test fashion.
%%% It also doesn't even use the gen server (it starts it but only to test the
%%% start sequence).

white_box_tests(_Config) -> 

	ct:log(sys_state, "white_box_tests: start~n"),

	ct:log(sys_state, "white_box_tests: starting gen_server~n"),
	{ok, _Pid} = hello_gen_server:start_link(),
	
	ct:log(sys_state, "white_box_tests: init gen_server~n"),
	{ok, _State} = hello_gen_server:init([]),
	
	ct:log(sys_state, "white_box_tests: test hello call~n"),
	{reply, world, dummy_state} = hello_gen_server:handle_call(hello, dummy_from, dummy_state),
	
	ct:log(sys_state, "white_box_tests: test stop call~n"),
	{stop, normal, ok, dummy_state} = hello_gen_server:handle_call(stop, dummy_from, dummy_state),
	
	ct:log(sys_state, "white_box_tests: test a bad call~n"),
	{reply, {error, invalid_call}, dummy_state} = hello_gen_server:handle_call(dummy_badcommand, dummy_from, dummy_state),
	
	ct:log(sys_state, "white_box_tests: test a cast~n"),
	{noreply, dummy_state} = hello_gen_server:handle_cast(dummy_msg, dummy_state),
	
	ct:log(sys_state, "white_box_tests: test an info call~n"),
	{noreply, dummy_state} = hello_gen_server:handle_info(dummy_info, dummy_state),
	
	ct:log(sys_state, "white_box_tests: test termination~n"),
	hello_gen_server:terminate(dummy_reason, dummy_state), % no check on return
	
	ct:log(sys_state, "white_box_tests: test code change call~n"),
	{ok, dummy_state} = hello_gen_server:code_change(dummy_old_vsn, dummy_state, dummy_extra).


%%% Functional test of the gen server calls from outside ('black box')
%%% This actually starts the server and then communicates with it.
	
black_box_tests(_Config) -> 

	%% "DIRECT PART": -------------------------------------------------

	ct:log(sys_state, "black_box_tests: start~n"),

	ct:log(sys_state, "black_box_tests: starting gen_server (directly, stand alone)~n"),
	{ok, GenServer} = hello_gen_server:start_link(),

	ct:log(sys_state, "black_box_tests: sending hello to gen_server~n"),
	world = gen_server:call(GenServer, hello),

	ct:log(sys_state, "black_box_tests: sending stop to gen_server~n"),
	ok = gen_server:call(GenServer, stop),

	ct:log(sys_state, "black_box_tests: gen_server stopped~n").
	

