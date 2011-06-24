Erlang Common Test Sample: Minimal White-Box Test
=================================================
*d_servertest*

This test suite tests a client and a server, namely the ones in ./client.erl 
and ./server.erl (A note: client and server API should generally reside in the
SAME file in Erlang, not as in this sample.).

The test suite is in test/server_SUITE.erl. It is minimal (not the full skeleton as in b).

Files in this directory:
------------------------

	  test/server_SUITE.erl		the actual test suite, used in test #3: 3_common_test.sh
	  
	  1_pathetic_test.sh		batch to test only client and server, w/o Common Test
	  2_spawn_test.sh			batch to test only client and server, w/o Common Test
	  3_common_test.sh			batch to test client and server using Common Test
	  
	  client.erl				the simple TCP client that is being tested
	  server.erl				the simple TCP server that is being tested
	  spawn.erl					helper program for test #2: 2_spawn_test.sh
	  
	  README.md					this file
	  original.erl				for reference only, source from 'Programming Erlang'
	  clean.sh					clean directory off logs, object files etc.
	  .gitignore				git version control suppport file


The functional part of the test suite are these lines:
------------------------------------------------------
	  
	  test/server_SUITE.erl:
	  ^^^^^^^^^^^^^^^^^^^^^
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
	  
This calls into the client and server module and starts the client and the server.

The first time around (my_test_case_1), the server is spawned and the call to the client is tested,
i.e. controlled for not being crashing.

The second time around (my_test_case_2), the client is spawned and the call to the server is tested,
again meaning: it is checked that the server does not crash. 
(That is the basis of how Common Test works.)
The client spawning is delayed by 3 seconds to make sure the server has enough 
time to start up before the client tries to connect.

'Unrelated Tests'
-----------------

There are two other tests here that have nothing to do with Common Test but
simply test the source of the client and the server because, well, in the end we want to 
test Common Test here, concretely, how it tests the client and server. So the client and
the server should better run, as expected.

These tests are started using 

	  ./1_pathetic_test.sh
	  ./2_spawn_test.sh

and the latter uses spawn.erl, which is a module that only exists to support that
second pre-test test.

Both these tests are somewhat interesting as they implement the briefest ways to
get two processes, a client and a server started from one call in the shell.
But that is only of tangential interest. Still, for the 'what you should see' output
of these, see below.

The main test, the Common Test, is started:
-------------------------------------------

	  ./3_common_test.sh

	  in eonbeam/dev/3/d_servertest


Or, in the same dir, with

	  erlc client.erl && erlc server.erl 
	  ct_run  -pa . -dir test


You have to have some patience. 

Then this is what you should see:
---------------------------------
	  
	  machine:~/eonbeam/dev/3/d_servertest you$ ./3_common_test.sh 
	  #3: Common Test
	  --- compiling ... ---
	  --- starting common test ... ---
	  Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]
	  
	  
	  Converting "." to "/Users/you/eonbeam/dev/3/d_servertest/." and re-inserting with add_patha/1
	  Eshell V5.8.2  (abort with ^G)
	  
	  (ct@machine)1> 
	  Common Test v1.5.2 starting (cwd is /Users/you/eonbeam/dev/3/d_servertest)
	  
	  
	  Common Test: Running make in test directories...
	  Recompile: server_SUITE
	  
	  CWD set to: "/Users/you/eonbeam/dev/3/d_servertest/ct_run.ct@machine.2011-06-14_19.07.45"
	  
	  TEST INFO: 1 test(s), 2 case(s) in 1 suite(s)
	  
	  Testing 3.d_servertest: Starting test, 2 test cases
	  Testing 3.d_servertest: TEST COMPLETE, 2 ok, 0 failed of 2 test cases
	  
	  Updating /Users/you/eonbeam/dev/3/d_servertest/index.html... done
	  Updating /Users/you/eonbeam/dev/3/d_servertest/all_runs.html... done
	  --- batch done ---

Check the output at

	  eonbeam/dev/3/d_servertest/index.html
	  

Clean the directory using 

	  ./clean.sh
	  
As with the previous tests, .gitignore is set to make git ignore all test result files.

Here is the output of the 'pre-tests without CT':
-------------------------------------------------

	  machine:~/eonbeam/dev/3/d_servertest you$ ./1_pathetic_test.sh 
	  #1: pathetic test (also try #2 & #3)
	  --- compiling ... ---
	  --- running server and client (without any CT) ---
	  --- starting server into background ... ---
	  --- and wait a bit ... ---
	  server started.
	  --- starting client ... ---
	  server got: <<"+++ This is some stuff sent from the client! +++">>
	  server done.
	  --- batch done ---

	  machine:~/eonbeam/dev/3/d_servertest you$ ./2_spawn_test.sh    
	  #2: spawn test (also try #1 & #3)
	  --- compiling ... ---
	  --- running server and client from a spawner process (without any CT) ---
	  starting spawner.
	  spawner done.
	  server started.
	  
	  =ERROR REPORT==== 14-Jun-2011::19:09:57 ===
	  Error in process <0.28.0> with exit value: {{badmatch,{error,eaddrinuse}},[{server,start,1}]}
	  
	  
	  =ERROR REPORT==== 14-Jun-2011::19:09:57 ===
	  Error in process <0.29.0> with exit value: {{badmatch,{error,econnrefused}},[{client,start,0}]}
	  
	  --- batch done ---
	  machine:~/eonbeam/dev/3/d_servertest you$ 
