Erlang Common Test Sample: Gen Server
=====================================
*f_genservertest*

This test suite tests a gen_server, namely the one in ./hello_gen_server.erl
The gen server is a full skeleton, with minimal specific functionality.

The test suite is in test/genserver_SUITE.erl. 
It is minimal (not the full skeleton as in b).

Files in this directory:
------------------------

	  1_pathetic_test.sh		batch to test only the gen server, w/o Common Test
	  2_common_test.sh			batch to test the gen server using Common Test
	  
	  test/genserver_SUITE.erl	the actual test suite, used in test #2: 2_common_test.sh

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
	  
The actual test in the test suite is a little more stuff. There are two tests,
a white box and a black box test:

The functional part of the Common Test are these lines:
-------------------------------------------------------
	  
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

There is another test here (1_pathetic_test.sh) that has nothing to do with Common 
Test but simply tests the source of the gen_server because, well, in the end we want to 
test Common Test here, concretely, how it tests gen_server. So the 
gen_server had better run as expected.

This test is started using 

	  ./1_pathetic_test.sh

and it uses hello.erl, which is a module that only exists to support that
pre-test test.

This test is very much like that in e_genserver/: a very brief gen_server sample,
with the client in hello.erl, the server in hello_gen_server.erl.

That is only of tangential interest here. 
Still, for the 'what you should see' of that test, see below.

The main test, the Common Test, is started:
-------------------------------------------

	  ./2_common_test.sh

	  in eonbeam/dev/3/f_genservertest

Or, in the same dir, with

	  erlc hello.erl && erlc hello_gen_server.erl 
	  ct_run  -pa . -dir test


Then this is what you should see:
---------------------------------
	  
	  machine:~/eonbeam/dev/3/f_genservertest you$ ./2_common_test.sh 
	  #2: Common Test of the Gen Server
	  --- compiling ... ---
	  --- starting common test ... ---
	  Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]
	  
	  
	  Converting "." to "/Users/you/eonbeam/dev/3/f_genservertest/." and re-inserting with add_patha/1
	  Eshell V5.8.2  (abort with ^G)
	  
	  (ct@machine)1> 
	  Common Test v1.5.2 starting (cwd is /Users/you/eonbeam/dev/3/f_genservertest)
	  
	  
	  Common Test: Running make in test directories...
	  Recompile: genserver_SUITE
	  
	  CWD set to: "/Users/you/eonbeam/dev/3/f_genservertest/ct_run.ct@machine.2011-06-19_20.50.34"
	  
	  TEST INFO: 1 test(s), 2 case(s) in 1 suite(s)
	  
	  Testing 3.f_genservertest: Starting test, 2 test cases
	  Testing 3.f_genservertest: TEST COMPLETE, 2 ok, 0 failed of 2 test cases
	  
	  Updating /Users/you/eonbeam/dev/3/f_genservertest/index.html... done
	  Updating /Users/you/eonbeam/dev/3/f_genservertest/all_runs.html... done
	  --- batch done ---

Check the output at

	  eonbeam/dev/3/f_genservertest/index.html
	  

Clean the directory using 

	  ./clean.sh
	  
As with the previous tests, .gitignore is set to make git ignore all test result files,
which are plenty.

Here is the output of the 'pre-tests without CT':
-------------------------------------------------

	  machine:~/eonbeam/dev/3/f_genservertest you$ 1_pathetic_test.sh 
	  #1: pathetic test (it's all about #2)
	  --- compiling ... ---
	  --- running ... ---
	  hello: starting gen_server
	  hello gen server: init
	  hello: sending hello to gen_server
	  hello gen server: hello received.
	  hello: sending stop to gen_server
	  hello gen server: stop received
	  hello gen server: terminating
	  hello: gen_server stopped
	  --- batch done ---
