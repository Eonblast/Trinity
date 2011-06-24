%%%-------------------------------------------------------------------
%%% File     : eonbeam/dev/3/f_genservertest/test/genserver_SUITE.erl
%%% Descr    : complete skeleton of a Common Test test suite 
%%% Author   : H. Diedrich
%%% Source   : Erlang docs 'small suite sample'
%%%            www.erlang.org/doc/apps/common_test/example_chapter.html
%%% Adapted  : 06/19/2011 hd
%%%-------------------------------------------------------------------
%%%
%%% Run: eonbeam/dev/3/f_genservertest/2_common_test.sh
%%% Or : ct_run -pa . -dir test (from eonbeam/dev/3/f_genservertest)
%%%
%%%-------------------------------------------------------------------

-module(genserver_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%% Mandatory list of test cases and test groups, and skip orders. 

all() -> 
    [white_box_tests, black_box_tests].


%% The test case. The return value is irrelevant. What matters is, if it returns.

%%% Unit tests of gen server call backs ('white box')
%%% This grapples directly with the individual SEQUENTIAL callback functions
%%% that implement the specifics of the concrete gen server at hand. It does
%%% NOT test the gen server as an entity but its parts, in unit test fashion.
%%% It also doesn't even use the gen server (it starts it but only to test the
%%% start sequence).

white_box_tests(_Config) -> 

	{ok, _Pid} = hello_gen_server:start_link(),
	
	{ok, _State} = hello_gen_server:init([]),
	
	{reply, world, dummy_state} = hello_gen_server:handle_call(hello, dummy_from, dummy_state),
	
	{stop, normal, ok, dummy_state} = hello_gen_server:handle_call(stop, dummy_from, dummy_state),
	
	{reply, {error, invalid_call}, dummy_state} = hello_gen_server:handle_call(dummy_badcommand, dummy_from, dummy_state),
	
	{noreply, dummy_state} = hello_gen_server:handle_cast(dummy_msg, dummy_state),
	
	{noreply, dummy_state} = hello_gen_server:handle_info(dummy_info, dummy_state),
	
	hello_gen_server:terminate(dummy_reason, dummy_state), % no check on return
	
	{ok, dummy_state} = hello_gen_server:code_change(dummy_old_vsn, dummy_state, dummy_extra).

%%% Functional test of the gen server calls from outside ('black box')
%%% This actually starts the server and then communicates with it.
	
black_box_tests(_Config) -> 

	% functional gen server test ('black box')
	{ok, GenServer} = hello_gen_server:start_link(),
	world = gen_server:call(GenServer, hello),
	ok = gen_server:call(GenServer, stop).
