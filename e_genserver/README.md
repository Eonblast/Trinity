Skeleton Gen Server
===================
*e_genserver*

This is no test suite sample, but demonstrates a skeleton of 
the Erlang Behavior **gen-server**.

Files in this directory:
------------------------

	  hello.sh					batch to compile and run the gen_server

	  hello.erl					the simple TCP client that is being tested
	  hello_gen_server.erl		the gen_server skeleton file, with a simple hello/world response
	  
	  README.md					this file
	  clean.sh					clean directory off logs, object files etc.
	  .gitignore				git version control suppport file


The functional part of are these lines:
---------------------------------------
	  
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


The server testdrive is started:
--------------------------------

	  ./test.sh

	  in eonbeam/dev/3/e_genserver

Or, in the same dir, with

	  erlc hello.erl && erlc hello_gen_server.erl 
	  erl -s hello run -s init stop -noshell

Then this is what you should see:
---------------------------------
	  
	  machine:~/eonbeam/dev/3/e_genserver you$ ./hello.sh
	  compiling ...
	  running ...
	  hello: starting gen_server
	  hello gen server: init
	  hello: sending hello to gen_server
	  hello gen server: hello received.
	  hello: sending stop to gen_server
	  hello gen server: stop received
	  hello gen server: terminating
	  hello: gen_server stopped

Clean the directory using 

	  ./clean.sh
	  
The .gitignore file is set to make git ignore various result files, not all of them used in this sample.

