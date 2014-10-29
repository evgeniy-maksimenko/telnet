-module(chat).
-include("../include/config.hrl").
-export([create/1]).
-export([get_all/0]).
-export([join/2]).

-record(chat_table,{
  key  = chats,
  tid  = chat_manager,
  file = chat_rooms,
  id   = 0
}).

join(Id, Ref)->
  C = #chat_table{},
  [BinaryId, _] = binary:split(Id, <<"\r\n">>),
  case ets:insert(C#chat_table.tid, {Ref, BinaryId}) of
    true ->
      list_to_binary("join to chat " ++ binary_to_list(Id));
    false ->
      <<"error join">>
  end.



get_all() ->
  C = #chat_table{},
  case filelib:is_file(C#chat_table.file) of
    false ->
      <<"no chats">>;
    true->
      dets:open_file(C#chat_table.file, [{type, bag}]),
      NewList = dets:lookup(C#chat_table.file, C#chat_table.key),
      list_to_binary(remove_key(NewList))
  end.

create(ChartName) ->
  C = #chat_table{},
  case filelib:is_file(C#chat_table.file) of
    false ->
      to_dets(false, C#chat_table.file, C#chat_table.key, ChartName);
    true ->
      to_dets(true, C#chat_table.file, C#chat_table.key, ChartName)
  end.

to_dets(true, File, Key, ChartName) ->
  dets:open_file(File, [{type, bag}]),
  [{_, Id, _}|_] = lists:reverse(lists:sort(dets:lookup(File, Key))),
  dets:insert(File, {Key, Id+1 , ChartName }),
  Res = "id | " ++ integer_to_list(Id + 1) ++ " | name |" ++ binary_to_list(ChartName),
  list_to_binary(Res);

to_dets(false, File, Key, ChartName) ->
  dets:open_file(File, [{type, bag}]),
  dets:insert(File, {Key, 0 , ChartName }),
  Res = "id | " ++ integer_to_list(0) ++ " | name |" ++ binary_to_list(ChartName),
  list_to_binary(Res).


remove_key(List) -> remove_key(List, []).
remove_key([], Acc) -> Acc;
remove_key([H | T], Acc) ->
  {_, Id, Name} = H,
  remove_key(T, [ "id | " ++ integer_to_list(Id) ++ " |  name | " ++ binary_to_list(Name) ++ "\n"  | Acc]).
