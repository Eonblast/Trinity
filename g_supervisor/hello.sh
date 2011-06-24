echo -n "compiling ."
erlc hello.erl
echo -n "."
erlc hello_gen_server.erl
echo -n "."
erlc hello_supervisor.erl 
echo 
echo "running ..."
erl -s hello run -s init stop -noshell
