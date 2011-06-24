echo "#3: Common Test"
echo "--- compiling ... ---"
erlc client.erl && erlc server.erl 
echo "--- starting common test ... ---"
ct_run  -pa . -dir test
echo "--- batch done ---"
