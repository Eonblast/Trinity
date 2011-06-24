%%%-------------------------------------------------------------------
%%% File     : eonbeam/dev/3/c_hellotest3/test/hello_SUITE.erl
%%% Descr    : complete skeleton of a Common Test test suite 
%%% Author   : H. Diedrich
%%% Source   : Erlang docs 'small suite sample'
%%%            www.erlang.org/doc/apps/common_test/example_chapter.html
%%% Adapted  : 06/13/2011 hd
%%% Requires : Erlang 14B (prior may not have ct_run)
%%%-------------------------------------------------------------------
%%%
%%% Run: eonbeam/dev/3/c_hellotest3/test.sh
%%% Or : ct_run -pa . -dir test (from eonbeam/dev/3/c_hellotest3)
%%%
%%%-------------------------------------------------------------------

-module(hello_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%% Mandatory list of test cases and test groups, and skip orders. 

all() -> 
    [my_test_case].
    

%% The test case. The return value is irrelevant. What matters is, if it returns.
%% In this case, it tests the return from hello:text("Test") to be as expected.
%% The '=' is Erlang pattern matching, which is, in effect, the test.

my_test_case(_Config) -> 
	  "Hello Test!~n" = hello:text("Test").
