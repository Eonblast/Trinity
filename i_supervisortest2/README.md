Erlang Common Test Sample: Supervisor and Gen Server
====================================================
*i_supervisortest2*

This test is almost identical to h, but for its adding of a child dynamically to
its supervisor. The child is the same as in h, the gen_server. 

The rest of this text is identical to h, except for places marked by **. The same
applies for the source: differences to h are rare and marked with **.

These **two** test suites test a gen_server, namely the one in ./hello_gen_server.erl,
and the supervisor that supervises (** but not: starts) and restarts that gen_server:
./hello_supervisor.erl. The gen server is a full skeleton, with minimal specific 
functionality, so is the supervisor.

The test suites are in test/genserver_SUITE.erl and test/supervisor_SUITE.erl. 
They are minimal (not the full skeleton as in b).

There is a preliminary test (1_pathetic_test) that does not use Common Test but
actually calls the gen_server directly, then the gen_server via supervisor,
to independently make sure that the modules work.

Files in this directory:
------------------------

	  1_pathetic_test.sh		batch to test supervisor and gen server, w/o Common Test
	  2_common_test.sh			batch to test supervisor and gen server WITH Common Test
	  
	  test/supervisor_SUITE.erl	one actual test suite, used in test #2: 2_common_test.sh
	  test/genserver_SUITE.erl	other actual test suite, used in test #2: 2_common_test.sh

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
		  % ** (different from h **)
		  {ok, SuperPid} = hello_supervisor:start_link(),                            **
		  {ok, _} = supervisor:start_child(SuperPid,                                 **
											      % child specs:                     **
			{hello_gen_server,                    % child id (a special kind of id!) **
 		     {hello_gen_server, start_link, []},  % MFA                              **
 		     permanent,                           % kind (permantent = always restart)
 		     1,                                   % max time to shutdown             **
 		     worker,                              % type                             **
 		     [hello_gen_server]                   % used modules (for code changes)  **
 		   }),                                                                       **

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


	  (this suite is identical to g, and h)
	  
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
the client in hello.erl supervises (not: starts) and connects to the server 
in hello_gen_server.erl, starting hello_supervisor, and then kicking the gen
server in manually.

For what you should see when running that test, see bottom of file.

The main test, the Common Test, is started:
-------------------------------------------

	  ./2_common_test.sh

	  in eonbeam/dev/3/i_supervisortest2

Or, in the same dir, with


	  erlc hello.erl && erlc hello_gen_server.erl && erlc hello_supervisor.erl
	  ct_run  -pa . -dir test


Then this is what you should see:
---------------------------------
(** The screen output of h and i is pretty much the same, but not the html logs.)
	  
	  machine:~/eonbeam/dev/3/i_supervisortest2 you$ 2_common_test.sh 
	  #2: Common Test of the Gen Server
	  --- compiling ... ---
	  --- starting common test ... ---
	  Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]
	  
	  
	  Converting "." to "/Users/you/eonbeam/dev/3/i_supervisortest2/." and re-inserting with add_patha/1
	  Eshell V5.8.2  (abort with ^G)
	  
	  (ct@machine)1> 
	  Common Test v1.5.2 starting (cwd is /Users/you/eonbeam/dev/3/i_supervisortest2)
	  
	  
	  Common Test: Running make in test directories...
	  
	  CWD set to: "/Users/you/eonbeam/dev/3/i_supervisortest2/ct_run.ct@machine.2011-06-23_14.33.25"
	  
	  TEST INFO: 1 test(s), 4 case(s) in 2 suite(s)
	  
	  Testing 3.i_supervisortest2: Starting test, 4 test cases
	  Testing 3.i_supervisortest2: TEST COMPLETE, 4 ok, 0 failed of 4 test cases
	  
	  Updating /Users/you/eonbeam/dev/3/i_supervisortest2/index.html... done
	  Updating /Users/you/eonbeam/dev/3/i_supervisortest2/all_runs.html... done
	  --- batch done ---

Check the output at

	  eonbeam/dev/3/i_supervisortest2/index.html

In this case, it's quite interesting. There are also ct:log calls added that show
in the HTML.

Clean the directory using 

	  ./clean.sh
	  
As with the previous tests, .gitignore is set to make git ignore all test result files,
which are plenty.

Here is the output of the 'pre-tests without CT':
-------------------------------------------------

	  machine:~/eonbeam/dev/3/i_supervisortest2 you$ 1_pathetic_test.sh 
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
	  hello: starting supervisor (will NOT start gen server) **
	  hello supervisor: start
	  hello supervisor: init (this one has no static child) **
	  hello: add dynamic gen server child to the supervisor **
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
