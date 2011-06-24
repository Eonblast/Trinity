echo "#2: Common Test of the Gen Server"

echo "--- compiling ... ---"
erlc hello.erl && erlc hello_gen_server.erl && erlc hello_supervisor.erl

echo "--- starting common test ... ---"
ct_run  -pa . -dir test

echo "--- batch done ---"
