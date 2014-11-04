-module(consumer_worker).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-include_lib("amqp_client/include/amqp_client.hrl").

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([receive_logs_direct/1]).

-define(SERVER, ?MODULE).

-record(state, {channel}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link([]) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Channel) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Channel], []).

receive_logs_direct(Argv) ->
  gen_server:cast(?MODULE,{receive_logs_direct, Argv}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([Channel]) ->
  receive_logs_direct(["show", "create", "join", "send"]),
  {ok, #state{channel=Channel}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast({receive_logs_direct, Argv}, State) ->
  amqp_channel:call(State#state.channel, #'exchange.declare'{exchange = <<"direct_logs">>,
    type = <<"direct">>}),

  #'queue.declare_ok'{queue = Queue} =
    amqp_channel:call(State#state.channel, #'queue.declare'{exclusive = true}),

  [amqp_channel:call(State#state.channel, #'queue.bind'{exchange = <<"direct_logs">>,
    routing_key = list_to_binary(Severity),
    queue = Queue})
    || Severity <- Argv],

  amqp_channel:subscribe(State#state.channel, #'basic.consume'{queue = Queue,
    no_ack = true}, self()),

  receive
    #'basic.consume_ok'{} -> ok
  end,

  loop(State#state.channel),
  {noreply, State};
handle_cast(_Request, State) ->
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

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec loop(Channel::pid())-> 'ok'.
loop(Channel) ->
  receive
    {#'basic.deliver'{routing_key = RoutingKey}, #amqp_msg{payload = Body}} ->
%%       io:format(" [x] Receive  ~p:~p~n", [RoutingKey, Body]),
      %% Запись в бд
      ets:insert(logs, {last_ets(lists:reverse(lists:sort(ets:tab2list(logs)))), RoutingKey, Body}),
      loop(Channel)
  end.

-spec last_ets(List::list()) -> Int::integer().
last_ets([])-> 0;
last_ets([{Id, _, _}|_]) -> Id + 1.



