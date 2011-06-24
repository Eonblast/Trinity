%%%-------------------------------------------------------------------
%%% File     : eonbeam/dev/3/d_servertest/test/server_SUITE.erl
%%% Descr    : complete skeleton of a Common Test test suite 
%%% Author   : H. Diedrich
%%% Source   : Erlang docs 'small suite sample'
%%%            www.erlang.org/doc/apps/common_test/example_chapter.html
%%% Adapted  : 06/14/2011 hd
%%% Requires : Erlang 14B (prior may not have ct_run)
%%%-------------------------------------------------------------------
%%%
%%% Run: eonbeam/dev/3/d_servertest/3_common_test.sh
%%% Or : ct_run -pa . -dir test (from eonbeam/dev/3/d_servertest)
%%%
%%%-------------------------------------------------------------------

-module(server_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").


%% Mandatory list of test cases and test groups, and skip orders. 

all() -> 
    [my_test_case_1, my_test_case_2].


%% The test case. The return value is irrelevant. What matters is, if it returns.
%% In this case, it tests the return from hello:text("Test") to be as expected.
%% The '=' is Erlang pattern matching, which is, in effect, the test.

my_test_case_1(_Config) -> 

	spawn(server, start, []),
	% wait out the server to come up
	receive after 3000 -> nil end,
	ok = client:start().
	
my_test_case_2(_Config) -> 

	% start the client, but delayed, so the server, below, starts first
	spawn(?MODULE, delayed_client_start, []),
	ok = server:start().
	
delayed_client_start() ->

	% wait out the server to come up
	receive after 3000 -> nil end,
	spawn(client, start, []).
