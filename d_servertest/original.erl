%% This is the original source from Erlang Programming, pg 329.
%% It is not used as is but put here as reference.

%% client -------------------------------------------------------------

client(Host, Data) ->

	{ok, Socket} = gen_tcp:connect(Host, 1234, [binary, {packet, 0}]),
	send(Socket, Data),
	ok = gen_tcp:close(Socket).

send(Socket, <<Chunk:100/binary, Rest/binary>>) ->

	gen_tcp:send(Socket, Chunk),
	send(Socket, Rest);

send(Socket, Rest) ->

	gen_tcp:send(Socket, Rest).
	
%% server -------------------------------------------------------------	

server() ->

	{ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}]),
	wait_connect(ListenSocket, 0).
	
wait_connect(ListenSocket, Count) ->

	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(?MODULE, wait_connect, [ListenSocket, Count+1]),
	get_request(Socket, [], Count).
	
get_request(Socket, BinaryList, Count) ->

	case gen_tcp:recv(Socket, 0, 5000) of
		{ok, Binary} ->
			get_request(Socket, [Binary|BinaryList], Count);
		{error, closed} ->
			handle(lists:reverse(BinaryList), Count)
	end.

handle(Binary, Count) ->

	{ok, Fd} -> file:open("log_file_" ++ integer_to_list(Count), write),
	file:write(Fd, Binary),
	file:close(Fd).
