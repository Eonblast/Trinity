Erlang Common Test Sample: Minimal Test
=======================================
*a_hellotest*

This test just returns "ok" to itself. It has no subject file that it runs etc.

The test suite is in test/hello_SUITE.erl. The source is really just:
	  
	  all() -> [my_test_case].

	  my_test_case(_Config) -> ok.

Start it with

	  ./test.sh
	
	  in eonbeam/dev/3/a_hellotest

Or, in the same dir, with

	  ct_run -dir test

This is what you should see:

	  machine:~/eonbeam/dev/3/a_hellotest you$ ct_run -dir test
	  Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]
	  
	  
	  
	  
	  Common Test v1.5.2 starting (cwd is /Users/you/eonbeam/dev/3/a_hellotest)
	  
	  Eshell V5.8.2  (abort with ^G)
	  (ct@machine)1> 
	  Common Test: Running make in test directories...
	  
	  CWD set to: "/Users/you/eonbeam/dev/3/a_hellotest/ct_run.ct@machine.2011-06-13_17.12.19"
	  
	  TEST INFO: 1 test(s), 1 case(s) in 1 suite(s)
	  
	  Testing 3.a_hellotest: Starting test, 1 test cases
	  Testing 3.a_hellotest: TEST COMPLETE, 1 ok, 0 failed of 1 test cases
	  
	  Updating /Users/you/eonbeam/dev/3/a_hellotest/index.html... done
	  Updating /Users/you/eonbeam/dev/3/a_hellotest/all_runs.html... done
	  
Check the output at

	  eonbeam/dev/3/a_hellotest/index.html
	  
Clean the directory using 

	  ./clean.sh