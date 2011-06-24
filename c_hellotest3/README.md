Erlang Common Test Sample: Minimal White-Box Test
=================================================
*c_hellotest3*

This test just calls a function in hello.erl and compares its return value.
So this hello.erl is the subject file that it runs on and tests as 'white box test', 
i.e. calling functions directly inside the tested module. This is different from
black box testing that only tests a system from the outside, using its official API.

The test suite is in test/hello_SUITE.erl. It is minimal (not the full skeleton as in b).

The tested file is ./hello.erl. 


The functional part are these lines:
	  
	  test/hello_SUITE.erl:
	  ^^^^^^^^^^^^^^^^^^^^^
	  all() -> [my_test_case].
	  my_test_case(_Config) -> "Hello Test!" = hello:text("Test").
	  
	  ./hello.erl:
	  ^^^^^^^^^^^^
	  text(T) -> "Hello " ++ T ++ "!~n".
	  

Start it with

	  ./test.sh
	
	  in eonbeam/dev/3/c_hellotest3


Or, in the same dir, with

	  erlc hello.erl
	  ct_run -dir test -pa .


This is what you should see:
	  
	  Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]
	  
	  
	  Converting "." to "/Users/you/eonbeam/dev/3/c_hellotest3/." and re-inserting with add_patha/1
	  Eshell V5.8.2  (abort with ^G)
	  
	  (ct@machine)1> 
	  Common Test v1.5.2 starting (cwd is /Users/you/eonbeam/dev/3/c_hellotest3)
	  
	  
	  Common Test: Running make in test directories...
	  
	  CWD set to: "/Users/you/eonbeam/dev/3/c_hellotest3/ct_run.ct@machine.2011-06-13_20.56.37"
	  
	  TEST INFO: 1 test(s), 1 case(s) in 1 suite(s)
	  
	  Testing 3.c_hellotest3: Starting test, 1 test cases
	  Testing 3.c_hellotest3: TEST COMPLETE, 1 ok, 0 failed of 1 test cases
	  
	  Updating /Users/you/eonbeam/dev/3/c_hellotest3/index.html... done
	  Updating /Users/you/eonbeam/dev/3/c_hellotest3/all_runs.html... done
	  machine:~/eonbeam/dev/3/c_hellotest3 you$ 


Check the output at

	  eonbeam/dev/3/c_hellotest3/index.html
	  

Clean the directory using 

	  ./clean.sh
	  
As with the hello tests a and b, .gitignore is set to make git ignore all test result files.