
-module(rp).
-behaviour(gen_server).
-behaviour(ranch_protocol).

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
  ok = Transport:setopts(Socket, [{active, once}]),
  gen_server:enter_loop(?MODULE, [],
    #state{socket=Socket, transport=Transport},
    ?TIMEOUT).
handle_info({tcp, Socket, Data}, State=#state{socket = Socket, transport=Transport}) ->
  Transport:setopts(Socket, [{active, once}]),
  case Data of
    <<"create/",Create/binary>> ->
      ?LOG_INFO("~p~n",[rp_chat:create(Create)]),
      Transport:send(Socket, Create);

    <<"show/",_/binary>> ->
      ?LOG_INFO("~p~n",[rp_chat:show()]),
      Transport:send(Socket, <<"ok">>);

    <<"join/",Id/binary>> ->
      ?LOG_INFO("~p~n",[rp_chat:join(self(), Id, Socket)]),
      Transport:send(Socket, Id);

    <<"send/",Msg/binary>> ->
      ?LOG_INFO("~p~n",[rp_chat:send(Msg)]);

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

