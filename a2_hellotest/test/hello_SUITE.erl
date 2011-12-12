%%%-------------------------------------------------------------------
%%% File    : erlang-arcana/b_hellotest2/test/hello_SUITE.erl
%%% Descr   : Most simple, crashong test suite to demonstrate Common Test
%%% Author  : H. Diedrich
%%% Source  : Erlang docs 'small suite sample'
%%%           www.erlang.org/doc/apps/common_test/example_chapter.html
%%% Adapted : 06/13/2011 hd
%%% Enhanced: 12/12/2011 hd
%%% Require : ct_run (sometimes not in path as it seems)
%%%-------------------------------------------------------------------
%%%
%%% Run: erlang-arcana/b_hellotest2/test.sh
%%% Or : ct_run -dir test (from erlang-arcana/b_hellotest2)
%%%
%%%-------------------------------------------------------------------

-module(hello_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%% The definition of the suite

all() -> 
    [my_test_case_1, my_test_case_2].


%% The test cases. The return value is irrelevant. What matters is, if it returns.
%% The '=' is Erlang pattern matching, which is, in effect, the test.

my_test_case_1(_Config) -> 
    ok.
    

my_test_case_2(_Config) -> 
	  "Foo" = "Bar".
