-module(pb_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).
-include("../../include/config.hrl").
-include_lib("stdlib/include/qlc.hrl").

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

%% ===================================================================
%% client functions
%% ===================================================================
-spec broadcast(PoolName::atom(),Msg::binary()) -> 'ok'.
broadcast(PoolName, Message)->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:cast(Worker, {broadcast, Message})
  end).

-spec show_chats(PoolName::atom()) ->
  Data::list() | {error, Failed::list()}.
show_chats(PoolName)->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:call(Worker, {show, all})
  end).

-spec create_chat(PoolName::atom(),Name::binary()) ->
  Out :: (atom() | list()).
create_chat(PoolName, Name) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:cast(Worker, {create, Name})
  end).

-spec join_chat(PoolName::atom(), Pid::pid(), Id::binary(), Ref::any()) -> 'true'.
join_chat(PoolName, Pid, Id, Ref) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:cast(Worker, {join, Pid, Id, Ref})
  end).

-spec(start_link([]) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link([]) ->
  gen_server:start_link(?MODULE, [], []).
%% ===================================================================
%% callback functions
%% ===================================================================
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({show, all},_From, State) ->
  Result = show(State#state.file, State#state.key),
  {reply, Result, State};
handle_call(_Request, _From, State) ->
  {reply, _Reply = ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
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

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec send(Msg::binary(), Tid::atom()) -> 'ok'.
send(Msg, Tid) ->
  SplitMsg = binary:split(Msg, <<" ">>),
  check_id(list_to_tuple(SplitMsg), Tid).

-spec check_id(List::list(), Tid::atom()) -> 'ok'.
check_id({Id, Msg}, Tid)  ->
  send_to_one(Tid, Msg, Id);
check_id({Msg}, Tid) ->
  send_to_all(Tid, Msg).

-spec send_to_all(Tid::atom(), Msg::binary()) -> 'ok'.
send_to_all(Tid, Msg) ->
  ListFromEts = qlc_ets(Tid),
  [Pid ! {tcp, Ref, Msg} || {Pid, Ref, _Id} <- ListFromEts ].

-spec send_to_one(Tid::atom(), Msg::binary(), Id::binary()) -> 'ok'.
send_to_one(Tid, Msg, ChooseId) ->
  ListFromEts = qlc_ets(Tid),
  [Pid ! {tcp, Ref, Msg} || {Pid, Ref, Id} <- ListFromEts, Id=:=ChooseId ].

-spec qlc_ets(Tid::atom()) -> Out::list().
qlc_ets(Tid) ->
  qlc:e(qlc:q([{PidInEts, RefInEts, IdInEts} || {PidInEts, RefInEts, IdInEts} <- ets:table(Tid)])).

-spec show(File::atom(),Key::atom()) ->
  Data::list() | {error, Failed::list()}.
show(File, Key) ->
  OpenFile = dets:open_file(File, [{type, bag}]),
  lookup_from_dets(OpenFile, File, Key).

-spec lookup_from_dets(OpenFile::tuple(),File::atom(),Key::atom()) ->
  Data::list() | {error, Failed::list()}.
lookup_from_dets({ok, _Reference}, File, Key) -> lookuped(dets:lookup(File, Key));
lookup_from_dets(Failed, _File, _Key) -> Failed.

-spec lookuped(In::list())-> Data::list() | {error, Failed::list()}.
lookuped({error, Failed}) -> {error, Failed};
lookuped(Data) -> Data.

-spec join(Pid::pid(),Id::binary(),Ref::any(),Tid::atom()) -> 'true'.
join(Pid, Id, Ref, Tid)->
  [BinaryId, _] = binary:split(Id, <<"\r\n">>),
  ets:insert(Tid, {Pid, Ref, BinaryId}).

-spec create(Name :: binary(), File::atom(), Key::atom()) ->
  Out :: (atom() | list()).
create(Name, File, Key) ->
  IsFile = filelib:is_file(File),
  to_dets(IsFile, File, Key, Name).

-spec to_dets(IsFileExists :: boolean(), File::atom(), Key::atom(), Name::binary()) ->
  Out :: (atom() | list()).
to_dets(IsFileExists, File, Key, Name) ->
  [SplitName, _]  = binary:split(Name, <<"\r\n">>),
  OpenFile        = dets:open_file(File, [{type, bag}]),
  save_to_dets(IsFileExists, OpenFile, File, Key, SplitName).

-spec save_to_dets(IsFileOpened::boolean(), OpenFile::tuple(), File::atom(), Key::atom(), ChatName::binary()) -> Out :: (atom() | list()).
save_to_dets(IsFileOpened, {ok, _Reference}, File, Key, ChatName) ->
  DetsLookup  = lists:reverse(lists:sort(dets:lookup(File, Key))),
  GetId       = get_id(IsFileOpened, isset_id(DetsLookup)),
  saved_in_dets(dets:insert(File, {Key, GetId , ChatName }));
save_to_dets( _,_ = Failed, _File, _Key, _SplitChatName) -> Failed.

-spec isset_id(List::list()) -> Id::integer().
isset_id([{_, Id, _}|_]) -> Id;
isset_id([]) -> 0.

-spec get_id(In :: boolean(), Id :: integer()) -> Out :: integer().
get_id(true, Id) -> Id +1;
get_id(false,_) -> 0.

-spec saved_in_dets(In :: (atom() | list())) -> Out :: (atom() | list()).
saved_in_dets(ok) -> ok;
saved_in_dets(FailedSave) -> FailedSave.

