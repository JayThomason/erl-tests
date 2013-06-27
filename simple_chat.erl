%% A simple message passing / chat application.
%%
%% Interface: simple_chat:start(UserName, FriendName).

-module(simple_chat).
-export([start/2, chatSend/1]).

start(UserName, FriendName) ->
  spawn(simple_chat, chatSend, [FriendName]).
%%  register(UserName, spawn(simple_chat, chatListen, [FriendName])).


chatListen(FriendName) ->
  receive
    {FromUserName, Message} when FromUserName == FriendName->
      io:format("~s: ~s~n", [FriendName, Message]),
      chatListen(FriendName)
  end.

chatSend(FriendName) ->
%%  timer:sleep(100),
%%  Message = io:get_line(">"),
%%  io:format("~s~n", [Message]),
  friend ! {"test"}.
