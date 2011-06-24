Skeleton OTP Application with Supervisor and Gen Server Worker Child
====================================================================
*j_application*

This is no test suite sample, but demonstrates a skeleton of 
the OTP **application**, with a sample skeleton **supervisor** and **gen-server**
as a worker child, a setup like sample h.

The word 'application' has special meaning 

Files in this directory:
------------------------

	  hello.sh					batch to compile and run the supervisor and gen_server child

	  hello.erl					the simple TCP client that is doing some tests 
	  hello_gen_server.erl		the gen_server skeleton file, with a simple hello/world response
	  hello_supervisor.erl		the supervisor skeleton file, that starts the gen_server
	  
	  README.md					this file
	  clean.sh					clean directory off logs, object files etc.
	  .gitignore				git version control suppport file


The 'functional' part are these lines:
--------------------------------------
	  
	  hello.erl:
	  ^^^^^^^^^^
	  run() -> ...
	  
		  {ok, GenServer} = hello_gen_server:start_link(), ...
		  world = gen_server:call(GenServer, hello), ...
		  ok = gen_server:call(GenServer, stop), ...

	
	  hello_gen_server.erl:
	  ^^^^^^^^^^^^^^^^^^^^^
	  handle_call(hello, _From, State) -> ...
	  {reply, world, State};


The supervisor and server testdrive is started:
-----------------------------------------------

	  ./hello.sh

	  in eonbeam/dev/3/j_application

Or, in the same dir, with

	  erlc hello.erl && erlc hello_gen_server.erl && erlc hello_supervisor.erl 
	  erl -s hello run -s init stop -noshell

Then this is what you should see:
---------------------------------
	  
	  machine:~/eonbeam/dev/3/j_application you$ ./hello.sh 
	  compiling ...
	  running ...
	  hello: starting supervisor
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

Clean the directory using 

	  ./clean.sh
	  
The .gitignore file is set to make git ignore various result files, not all of them used in this sample.

