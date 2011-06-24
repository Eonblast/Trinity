Erlang Common Test Sample: Complete Test Suite Skeleton
=======================================================
*b_hellotest2*

This test just returns "ok" to itself. It has no subject file that it runs etc.

The test suite is in test/hello_SUITE.erl. 

It functions exactly like a_hellotest.

But the source has many comments and all optional functions that control a test suite:

	  all() %% only mandatory function
	  suite()
	  init_per_suite(Config)
	  end_per_suite(_Config)
	  init_per_group(_GroupName, Config)
	  end_per_group(_GroupName, _Config)
	  init_per_testcase(_TestCase, Config)
	  end_per_testcase(_TestCase, _Config)
	  groups()

But the functional part is really just:
	  
	  all() -> [my_test_case].
	  my_test_case(_Config) -> ok.

Start it with

	  ./test.sh
	
	  in eonbeam/dev/3/b_hellotest2

Or, in the same dir, with

	  ct_run -dir test

This is what you should see:
	  
	  Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]
	  
	  
	  
	  
	  Common Test v1.5.2 starting (cwd is /Users/you/eonbeam/dev/3/b_hellotest2)
	  
	  Eshell V5.8.2  (abort with ^G)
	  (ct@machine)1> 
	  Common Test: Running make in test directories...
	  Recompile: hello_SUITE
	  
	  CWD set to: "/Users/you/eonbeam/dev/3/b_hellotest2/ct_run.ct@machine.2011-06-13_19.05.30"
	  
	  TEST INFO: 1 test(s), 1 case(s) in 1 suite(s)
	  
	  Testing 3.b_hellotest2: Starting test, 1 test cases
	  Testing 3.b_hellotest2: TEST COMPLETE, 1 ok, 0 failed of 1 test cases
	  
	  Updating /Users/you/eonbeam/dev/3/b_hellotest2/index.html... done
	  Updating /Users/you/eonbeam/dev/3/b_hellotest2/all_runs.html... done	  

Check the output at

	  eonbeam/dev/3/b_hellotest2/index.html
	  
Clean the directory using 

	  ./clean.sh