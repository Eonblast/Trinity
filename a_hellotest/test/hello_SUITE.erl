%%%-------------------------------------------------------------------
%%% File    : eonbeam/dev/3/a_hellotest/test/hello_SUITE.erl
%%% Description : Most simple test suite to demonstrate Common Test
%%% Author  : H. Diedrich
%%% Source  : Erlang docs 'small suite sample'
%%%           www.erlang.org/doc/apps/common_test/example_chapter.html
%%% Adapted : 06/13/2011 hd
%%% Require : ct_run (sometimes not in path as it seems)
%%%-------------------------------------------------------------------
%%%
%%% Run: eonbeam/dev/3/a_hellotest/test.sh
%%% Or : ct_run -dir test (from eonbeam/dev/3/a_hellotest)
%%%
%%%-------------------------------------------------------------------

-module(hello_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------

all() -> 
    [my_test_case].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------

my_test_case(_Config) -> 
    ok.