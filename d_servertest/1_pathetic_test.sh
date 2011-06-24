echo "#1: pathetic test (also try #2 & #3)"
echo "--- compiling ... ---"
erlc client.erl && erlc server.erl 
echo "--- running server and client (without any CT) ---"
echo "--- starting server into background ... ---"
erl -pa ./ebin -s server start -s init stop -noshell -noinput &
echo "--- and wait a bit ... ---"
sleep 3
echo "--- starting client ... ---"
erl -s client start -s init stop -noshell
echo "--- batch done ---"
