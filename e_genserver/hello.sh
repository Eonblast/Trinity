echo "compiling ..."
erlc hello.erl && erlc hello_gen_server.erl 
echo "running ..."
erl -s hello run -s init stop -noshell
