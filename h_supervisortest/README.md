Erlang Common Test Sample: Supervisor and Gen Server
====================================================
*h_supervisortest*

These **two** test suites test a gen_server, namely the one in ./hello_gen_server.erl,
and the supervisor that starts and restarts that gen_server: ./hello_supervisor.erl.
The gen server is a full skeleton, with minimal specific functionality,
so is the supervisor.

The test suites are in test/genserver_SUITE.erl and test/supervisor_SUITE.erl. 
They are minimal (not the full skeleton as in b).

There is a preliminary test (1_pathetic_test) that does not use Common Test but
actually calls the gen_server directly, then the gen_server via supervisor,
to independently make sure that modules work.

Files in this directory:
------------------------

	  1_pathetic_test.sh		batch to test supervisor and gen server, w/o Common Test
	  2_common_test.sh			batch to test supervisor and gen server WITH Common Test
	  
	  test/supervisor_SUITE.erl	other actual test suite, used in test #2: 2_common_test.sh
	  test/genserver_SUITE.erl	one actual test suite, used in test #2: 2_common_test.sh

	  hello_supervisor.erl		the simple supervisor that is being tested
	  hello_gen_server.erl		the simple gen server that is being tested

	  hello.erl					test client for (pre-)test #1: 1_pathetic_test.sh
	  
	  README.md					this file
	  clean.sh					clean directory off logs, object files etc.
	  .gitignore				git version control suppport file


The functional part of the gen server are these lines:
------------------------------------------------------
	  
	  hello_gen_server.erl:
	  ^^^^^^^^^^^^^^^^^^^^^
	  handle_call(hello, _From, State) ->
	 	  ...
		  {reply, world, State};
	  
The actual tests in the test suites are quite some lines. There are two types of tests,
white box and black box test, for the gen_server. The supervisor is also tested as 
whitebox but that makes not that much sense obviously. It's more interesting to test
the functional side, i.e. 'black box'. And so for the supervisor suite, that black box
testing has more calls than the white box one. It also checks automatic restarts.

The functional part of the Common Tests are these lines:
--------------------------------------------------------

	  (this suite has the new stuff)

	  test/supervisor_SUITE.erl:
	  ^^^^^^^^^^^^^^^^^^^^^^^^^

	  %%% Unit tests of gen server call backs ('white box')
	  %%% White box testing grapples directly with the individual SEQUENTIAL callback 
	  %%% functions, which is not that exciting for a supervisor. By definition there
	  %%% should be at best no functionality in a supervisor, so nothing to test.
	  %%% We do test the expected child spec, maybe out of place but could come handy.
	  
	  white_box_tests(_Config) -> 
	  
		  % check start_link/0
		  {ok, SuperPid} = hello_supervisor:start_link(),
		  receive after 100 -> nil end, % let start
		  exit(SuperPid, normal),
	  
		  % check init/1
		  ExpectedSupervisorSpec = ...
		  ExpectedChildSpec = ...
		  {ok, {ExpectedSupervisorSpec, ExpectedChildSpec}} =
			  hello_supervisor:init([]),
		  
		  % check child spec syntax
		  ok = supervisor:check_childspecs(ExpectedChildSpec),
	  
	  %%% Functional test of the gen server calls from outside ('black box')
	  %%% This actually starts the server and then communicates with it.
		  
	  black_box_tests(_Config) -> 
	  
		  process_flag(trap_exit, true),
		  ...
		  {ok, _} = hello_supervisor:start_link(),
		  world = gen_server:call(hello_gen_server, hello),
		  ok = gen_server:call(hello_gen_server, stop),
		  ...
		  % check that the gen server is in fact running again
		  true = erlang:is_process_alive(whereis(hello_gen_server)),
		  ...
		  % test a message
		  world = gen_server:call(hello_gen_server, hello),
		  ok = gen_server:call(hello_gen_server, stop),
		  ...


	  (this suite is identical to g)
	  
	  test/genserver_SUITE.erl:
	  ^^^^^^^^^^^^^^^^^^^^^^^^^

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
	  
		  {ok, GenServer} = hello_gen_server:start_link(),
		  world = gen_server:call(GenServer, hello),
		  ok = gen_server:call(GenServer, stop).

'Unrelated Test'
----------------

There is preliminary test here (1_pathetic_test.sh) that has nothing to do with Common 
Test but simply tests the source of the gen_server and supervisor modules, because in the
end we want to test Common Test here, concretely, how it tests gen_server. So the 
gen_server and supervisor had better run as expected.

This test is started using 

	  ./1_pathetic_test.sh

and it uses hello.erl, which is a module that only exists to support that
pre-test test. Its source is much like that used in the test suites:
the client in hello.erl starts and connects to the server in hello_gen_server.erl,
using hello_supervisor to kick the server in.

For what you should see when running that test, see bottom of file.

The main test, the Common Test, is started:
-------------------------------------------

	  ./2_common_test.sh

	  in eonbeam/dev/3/h_supervisortest

Or, in the same dir, with


	  erlc hello.erl && erlc hello_gen_server.erl && erlc hello_supervisor.erl
	  ct_run  -pa . -dir test


Then this is what you should see:
---------------------------------
	  
	  machine:~/eonbeam/dev/3/h_supervisortest me$ 2_common_test.sh 
	  #2: Common Test of the Gen Server
	  --- compiling ... ---
	  --- starting common test ... ---
	  Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]
	  
	  
	  Converting "." to "/Users/me/eonbeam/dev/3/h_supervisortest/." and re-inserting with add_patha/1
	  Eshell V5.8.2  (abort with ^G)
	  
	  (ct@machine)1> 
	  Common Test v1.5.2 starting (cwd is /Users/me/eonbeam/dev/3/h_supervisortest)
	  
	  
	  Common Test: Running make in test directories...
	  Recompile: supervisor_SUITE
	  
	  CWD set to: "/Users/me/eonbeam/dev/3/h_supervisortest/ct_run.ct@machine.2011-06-22_18.44.04"
	  
	  TEST INFO: 1 test(s), 4 case(s) in 2 suite(s)
	  
	  Testing 3.h_supervisortest: Starting test, 4 test cases
	  Testing 3.h_supervisortest: TEST COMPLETE, 4 ok, 0 failed of 4 test cases
	  
	  Updating /Users/me/eonbeam/dev/3/h_supervisortest/index.html... done
	  Updating /Users/me/eonbeam/dev/3/h_supervisortest/all_runs.html... done
	  --- batch done ---

Check the output at

	  eonbeam/dev/3/h_supervisortest/index.html

In this case, it's quite interesting. There are also ct:log calls added that show
in the HTML.

Clean the directory using 

	  ./clean.sh
	  
As with the previous tests, .gitignore is set to make git ignore all test result files,
which are plenty.

Here is the output of the 'pre-tests without CT':
-------------------------------------------------

	  machine:~/eonbeam/dev/3/h_supervisortest you$ ./1_pathetic_test.sh 
	  #1: pathetic test (it's all about #2)
	  --- compiling ... ---
	  --- running ... ---
	  hello: starting gen_server (directly, stand alone)
	  hello gen server: init
	  hello: sending hello to gen_server
	  hello gen server: hello received.
	  hello: sending stop to gen_server
	  hello gen server: stop received
	  hello gen server: terminating
	  hello: gen_server stopped
	  - - -
	  hello: starting supervisor (will start gen server)
	  hello supervisor: start
	  hello supervisor: init
	  hello gen server: init
	  hello: sending hello to gen_server
	  hello gen server: hello received.
	  hello: sending stop to gen_server
	  hello gen server: stop received
	  hello gen server: terminating
	  hello: gen_server stopped (but the supervisor should restart it)
	  hello gen server: init
	  hello: sending hello to gen_server
	  hello gen server: hello received.
	  hello: sending stop to gen_server
	  hello gen server: stop received
	  hello gen server: terminating
	  hello: gen_server stopped (and now the supervisor should NOT restart it)
	  hello: bingo
	  --- batch done ---