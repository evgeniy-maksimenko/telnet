-module(rp).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% @doc telnet chat-server
%% What can you do
%% crete/new room
%% show/
%% join/1
%% send/1 text
%% send/text
%% @end

-include("../include/config.hrl").
-export([start_link/4]).

-export([init/1]).
-export([init/4]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-define(TIMEOUT, 60000).
-record(state, {socket, transport}).

start_link(Ref, Socket, Transport, Opts) ->

  proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init([]) -> {ok, undefined}.
init(Ref, Socket, Transport, _Opts = []) ->

  ets:insert(chat_manager, {self(), Socket, 1}),

  ok = proc_lib:init_ack({ok, self()}),
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, true}]),
  gen_server:enter_loop(?MODULE, [],
    #state{socket=Socket, transport=Transport},
    ?TIMEOUT).
handle_info({tcp, Socket, Data}, State=#state{socket = Socket, transport=Transport}) ->
  Transport:setopts(Socket, [{active, once}]),
  PoolName = pool1,
  case Data of
    <<"create/",Create/binary>> ->
      pb_worker:create_chat(PoolName, Create),
      Transport:send(Socket, <<"create ",Create/binary>>);

    <<"show/",_/binary>> ->
      Object = pb_worker:show_chats(PoolName),
      ShowOut = show_in(Object),
      Transport:send(Socket, list_to_binary(ShowOut));

    <<"join/",Id/binary>> ->
      pb_worker:join_chat(PoolName, self(), Id, Socket),
      Transport:send(Socket, <<"join to ",Id/binary>>);

    <<"send/",Msg/binary>> ->
      pb_worker:broadcast(PoolName, Msg);

    Data ->
      Transport:send(Socket, Data)
  end,

  {noreply, State, ?TIMEOUT};

handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
  {stop, Reason, State};

handle_info(timeout, State) ->
  {stop, normal, State};

handle_info(_Info, State) ->
  {stop, normal, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

show_in(Object) ->
  case Object of
    {ok,List} ->
      remove_key(List);
    {error, _Reason} ->
      "error, no rooms"
  end.


remove_key(List) -> remove_key(List, []).
remove_key([], Acc) -> Acc;
remove_key([H | T], Acc) ->
 {_, Id, Name} = H,
 remove_key(T, [ "id | " ++ integer_to_list(Id) ++ " | name | " ++ binary_to_list(Name) ++ "\r\n" | Acc]).

