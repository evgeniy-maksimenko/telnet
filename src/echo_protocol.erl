-module(echo_protocol).
-behaviour(ranch_protocol).
-include("../include/config.hrl").
-export([start_link/4]).
-export([init/4]).
-export([send/2]).


-record(chat_table,{
  tid  = chat_manager
}).

start_link(Ref, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
  {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
  ok = ranch:accept_ack(Ref),
  loop(Socket, Transport).

loop(Socket, Transport) ->
  case Transport:recv(Socket, 0, 60000) of

    {ok, <<"CREATE CHAT ", ChatName/binary>>} ->
      CreateChat = chat:create(ChatName),
      Transport:send(Socket, CreateChat),
      loop(Socket, Transport);

    {ok, <<"SHOW CHATS", _/binary>>} ->
      GetChats = chat:get_all(),
      Transport:send(Socket, GetChats),
      loop(Socket, Transport);

    {ok, <<"JOIN CHAT ", Id/binary>>} ->
      JoinChat = chat:join(Id, Socket),
      Transport:send(Socket, JoinChat),
      ?LOG_INFO("~p~n",ets:tab2list(chat_manager)),
      loop(Socket, Transport);

    {ok, <<"SEND CHAT ", Msg/binary>>} ->
      pool_send(Msg),
      loop(Socket, Transport);

    {ok, Data} ->
      Transport:send(Socket, Data),
      loop(Socket, Transport);

    _ ->
      ok = Transport:close(Socket)
  end.

send(Socket, Data) ->
  ranch_tcp:send(Socket, Data),
  loop(Socket, ranch_tcp).


pool_send(Msg) ->
  C = #chat_table{},
  SplitMsg = case binary:split(Msg, <<" ">>) of
    [Id, Message] ->
      [BinaryId] = binary:split(Id, <<"\r\n">>),
      bang(ets:first(C#chat_table.tid), BinaryId, Message, C#chat_table.tid)
  end,
  SplitMsg.

bang('$end_of_table',  _BinaryId, _Msg, _Tab) -> ok;
bang(Firts,  BinaryId, Msg, Tab) ->
  case ets:lookup(Tab, Firts) of
    [{Ref, BinaryId}] ->
      send(Ref, Msg);
    _ ->
      ok
  end,
  bang(ets:next(Tab, Firts), BinaryId, Msg, Tab).

