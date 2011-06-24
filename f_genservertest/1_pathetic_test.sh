echo "#1: pathetic test (it's all about #2)"
echo "--- compiling ... ---"
erlc hello.erl && erlc hello_gen_server.erl 
echo "--- running ... ---"
erl -s hello run -s init stop -noshell
echo "--- batch done ---"