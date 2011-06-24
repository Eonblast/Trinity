echo "#2: spawn test (also try #1 & #3)"
echo "--- compiling ... ---"
erlc client.erl && erlc server.erl && erlc spawn.erl 
echo "--- running server and client from a spawner process (without any CT) ---"
erl -s spawn start -s init stop -noshell
echo "--- batch done ---"
