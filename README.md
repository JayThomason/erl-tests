erl-tests
=========

Simple Erlang tests meant to be compared to the chat tests written in Waldo, 
which can be found [here](https://github.com/JayThomason/waldo-tests).

simple_chat.erl is a two user chat while multi_chat.erl is a basic chatroom.

Simple_chat: each user must know the erlang node and username of the other
user. Then the users can connect with simple_chat:start(username, friendname,
friendnode).

Similarly, in multi_chat the client must know the node of the server's node. 
The server is started with multi_chat:server_start(). Clients can then connect
to this server using multi_chat:client_start(username, servernode).
