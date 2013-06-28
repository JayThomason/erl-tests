%%
%% Multi-user chat application using a client/server model.
%%

-module(multi_chat).
-export([server_start/0, server_listen/1, 
         client_start/2, client_listen/2]).

server_start() ->
  register(server, spawn(?MODULE, server_listen, [[]])).

%% Tail recursive server listen. Allows logging on, logging off, and 
%% sending messages to all other users logged on.
server_listen(Users) ->
  io:format("Num users online: ~p~n", [length(Users)]),
  receive
    {Username, Node, logon} ->
      io:format("logon: ~p~n", [Username]),
      server_listen(Users ++ [{Username, Node}]);
    {Username, Node, logoff} ->
      io:format("logoff: ~p~n", [Username]),
      server_listen(Users -- [{Username, Node}]);
    {Username, Node, Message} ->
      io:format("~p> ~s", [Username, Message]),
      server_broadcast_message(Username, Message, Users),
      server_listen(Users);
    _ ->
      io:format("recvd something...~n", []),
      server_listen(Users)
  end.

%% Sends broadcasts a message to every user logged on.
server_broadcast_message(Username, Message, []) ->
  ok;
server_broadcast_message(Username, Message, [Head|Tail]) ->
  {CurrUser, Node} = Head,
  if 
    CurrUser == Username ->
      ok;
    CurrUser /= Username ->
      Head ! {Username, Message, message}
  end,
  server_broadcast_message(Username, Message, Tail).

client_start(User, ServerNode) ->
  register(User, spawn(?MODULE, client_listen, [User, ServerNode])),
  {server, ServerNode} ! {User, node(), logon},
  client_send_messages(User, ServerNode).

%% Listens for messages broadcast by the server.
client_listen(User, ServerNode) ->
  receive
    {Username, Message, message} ->
      io:format("~p> ~s", [Username, Message]),
      client_listen(User, ServerNode);
    _ ->
      client_listen(User, ServerNode)
  end.

%% Sends messages from the client to the server for broadcast.
%% Logs the user off when they type 'logoff'.
client_send_messages(User, ServerNode) ->
  Msg = io:get_line("msg: "),
  LogOff = string:equal(Msg, "logoff\n"),
  if 
    LogOff ->
      {server, ServerNode} ! {User, node(), logoff};
    not LogOff ->
      {server, ServerNode} ! {User, node(), Msg},
      client_send_messages(User, ServerNode)
  end. 
