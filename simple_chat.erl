%%
%% A simple message passing / chat application.
%%

-module(simple_chat).
-export([start/3, listenForMessages/0]).

start(User, Friend, FriendNode) ->
  register(User, spawn(?MODULE, listenForMessages, [])),
  io:format("Chatting with ~p. Type 'finished' to exit.~n", [Friend]),
  sendMessages(false, Friend, FriendNode).

listenForMessages() ->
  receive
    {"finished\n"} ->
      io:format("Chat session finished.~n", []),
      exit(normal);
    {Message} -> 
      io:format("~s", [Message]),
      listenForMessages()
  end.

sendMessages(Finished, Friend, FriendNode) ->
  if 
    Finished ->
      exit(normal);
    not Finished ->
      ok
  end,
  Message = io:get_line(">"),
  {Friend, FriendNode} ! {Message},
  sendMessages(string:equal(Message, "finished\n"), Friend, FriendNode).
