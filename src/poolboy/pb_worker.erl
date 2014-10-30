-module(pb_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).
-include("../../include/config.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3]).

-export([broadcast/2]).
-export([create_chat/2]).
-export([show_chats/1]).
-export([join_chat/4]).

-record(state,{
  tid  = chat_manager :: atom(),
  file = chat_rooms :: atom(),
  key  = chats :: atom(),
  id   = 0 :: integer()
}).

-type answer() :: {ok, Reference::list()} | {error, Reason::list()}.
%% ===================================================================
%% client functions
%% ===================================================================
broadcast(PoolName, Message)->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:cast(Worker, {broadcast, Message})
  end).

show_chats(PoolName)->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:call(Worker, {show, all})
  end).

create_chat(PoolName, Name) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:cast(Worker, {create, Name})
  end).

join_chat(PoolName, Pid, Id, Ref) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:cast(Worker, {join, Pid, Id, Ref})
  end).


start_link([]) ->
  gen_server:start_link(?MODULE, [], []).
%% ===================================================================
%% callback functions
%% ===================================================================
init([]) ->
  {ok, #state{}}.

handle_call({show, all},_From, State) ->
  Result = show(State#state.file, State#state.key),
  {reply, Result, State};

handle_call(_Request, _From, State) ->
  {reply, _Reply = ok, State}.

handle_cast({join, Pid, Id, Ref}, State) ->
  join(Pid, Id, Ref, State#state.tid),
  {noreply, State};

handle_cast({create, Name}, State) ->
  create(Name, State#state.file, State#state.key),
  {noreply, State};

handle_cast({broadcast, Message}, State) ->
  send(Message, State#state.tid),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

send(Msg, Tid) ->
  case binary:split(Msg, <<" ">>) of
    [Id, Message] ->
      bang(ets:first(Tid), Id, Message, Tid);
    [Message] ->
      bang(ets:first(Tid), [], Message, Tid)
  end.

bang('$end_of_table',_Id, _Msg, _Tab) -> ok;
bang(Firts, Id, Msg, Tab) ->
  MessageSendingRes = case Id of
                        [] ->
                          case ets:lookup(Tab, Firts) of
                            [{PidNew, RefNew, _}] ->
                              PidNew ! {tcp, RefNew, Msg};
                            _ ->
                              {ok, send_msg, Msg}
                          end;
                        Id ->
                          case ets:lookup(Tab, Firts) of
                            [{PidNew2, RefNew2, Id}] ->
                              PidNew2 ! {tcp, RefNew2, Msg};
                            _ ->
                              {ok, send_msg, Msg}
                          end
                      end,
  io:format("sending message result ~p~n", [MessageSendingRes]),
  bang(ets:next(Tab, Firts), Id, Msg, Tab).

-spec show(File::atom(), Key::atom())-> answer().
show(File, Key) ->
  case filelib:is_file(File) of
    true->
      case dets:open_file(File, [{type, bag}]) of
        {ok, _Reference} ->
          case dets:lookup(File, Key) of
            {error, ErrorLookup} ->
              {error, ErrorLookup};
            Object ->
              {ok, Object}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    false ->
      {error, file_not_found}
  end.

-spec join(Pid::pid(),Id::binary(),Ref::char(), Tid::atom()) ->  {ok, insert} | {error, faile_insert}.
join(Pid, Id, Ref, Tid)->
  [BinaryId, _] = binary:split(Id, <<"\r\n">>),
  case ets:insert(Tid, {Pid, Ref, BinaryId}) of
    true ->
      {ok, insert}
  end.

-spec create(ChartName :: binary(), File::atom(), Key::atom()) ->
  {ok, Reference::list()} | {error, Reason::list()}.
create(ChartName, File, Key) ->
  case filelib:is_file(File) of
    false ->
      to_dets(false, File, Key, ChartName);
    true ->
      to_dets(true, File, Key, ChartName)
  end.

-spec to_dets(IsFileExists::boolean(), File::atom(), Key::atom(), ChartName::binary()) -> answer().
to_dets(IsFileExists, File, Key, ChartName) ->
  [ChartNameSplit, _] = binary:split(ChartName, <<"\r\n">>),
  case dets:open_file(File, [{type, bag}]) of
    {ok, _Reference} ->
      case IsFileExists of
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