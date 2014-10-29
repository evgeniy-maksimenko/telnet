
-module(rp_chat).
-include("../include/config.hrl").
-record(chat_table,{
  tid  = chat_manager,
  file = chat_rooms,
  key  = chats,
  id   = 0
}).
-export([create/1]).
-export([show/0]).
-export([join/3]).
-export([send/1]).

create(ChartName) ->
  C = #chat_table{},
  case filelib:is_file(C#chat_table.file) of
    false ->
      to_dets(false, C#chat_table.file, C#chat_table.key, ChartName);
    true ->
      to_dets(true, C#chat_table.file, C#chat_table.key, ChartName)
  end.

to_dets(Method, File, Key, ChartName) ->
  [ChartNameSplit, _] = binary:split(ChartName, <<"\r\n">>),
  case dets:open_file(File, [{type, bag}]) of
    {ok, _Reference} ->
      case Method of
        true->
          [{_, Id, _}|_] = lists:reverse(lists:sort(dets:lookup(File, Key))),
          case dets:insert(File, {Key, Id+1 , ChartNameSplit }) of
            ok ->
              {ok, {create_chat, ChartNameSplit}};
            {error, ErrorInsert} ->
              {error, ErrorInsert}
          end;
        false ->
          case dets:insert(File, {Key, 0 , ChartNameSplit }) of
            ok ->
              {ok, {create_chat, ChartNameSplit}};
            {error, ErrorInsertFirst} ->
              {error, ErrorInsertFirst}
          end
      end;
    {error, ReasonOpenFile} ->
      {error, ReasonOpenFile}
  end.

show() ->
  C = #chat_table{},
  case filelib:is_file(C#chat_table.file) of
    false ->
      {error, file_not_found};
    true->
      case dets:open_file(C#chat_table.file, [{type, bag}]) of
        {ok, _Reference} ->
          case dets:lookup(C#chat_table.file, C#chat_table.key) of
            Object ->
              {ok, Object};
            {error, ErrorLookup} ->
              {error, ErrorLookup}
          end;
        {error, Reason} ->
          {error, Reason}
      end
  end.

join(Pid, Id, Ref)->
  C = #chat_table{},
  [BinaryId, _] = binary:split(Id, <<"\r\n">>),
  case ets:insert(C#chat_table.tid, {Pid, Ref, BinaryId}) of
    true ->
      {ok, insert};
    false ->
      {error, faile_insert}
  end.

send(Msg) ->
  C = #chat_table{},
  case binary:split(Msg, <<" ">>) of
    [Id, Message] ->
      [BinaryId] = binary:split(Id, <<"\r\n">>),
      bang(ets:first(C#chat_table.tid),BinaryId, Message, C#chat_table.tid);
    [Message] ->
      bang(ets:first(C#chat_table.tid), [], Message, C#chat_table.tid)
  end.

bang('$end_of_table',_Id, _Msg, _Tab) -> ok;
bang(Firts, Id, Msg, Tab) ->
  case Id of
    [] ->
      case ets:lookup(Tab, Firts) of
        [{PidNew, RefNew, _}] ->
          PidNew ! {tcp, RefNew, Msg};
        _ ->
          {ok, send_msg, Msg}
      end;
    Id ->
      case ets:lookup(Tab, Firts) of
        [{PidNew, RefNew, Id}] ->
          PidNew ! {tcp, RefNew, Msg};
        _ ->
          {ok, send_msg, Msg}
      end
  end,
  bang(ets:next(Tab, Firts), Id, Msg, Tab).
