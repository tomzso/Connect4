-module(chat_server).
-export([start/0, accept/1, handle_client/1]).

start() ->
  {ok, Listen} = gen_tcp:listen(5555, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {ip, {0,0,0,0}}]),
  io:format("Server listening on port 5555~n"),
  accept(Listen).

accept(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> handle_client(Socket) end),
  accept(Listen).

handle_client(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      io:format("Received: ~p~n", [Data]),
      gen_tcp:send(Socket, << "Echo: ", Data/binary >>),
      handle_client(Socket);
    {error, closed} ->
      io:format("Client disconnected~n"),
      ok
  end.
