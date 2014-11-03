-module(broker_worker).
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

-export([send_logs_direct/1]).

-define(SERVER, ?MODULE).

-record(state, {
  connect :: atom()
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link([]) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Channel) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Channel], []).

send_logs_direct(Argv) ->
  gen_server:cast(?MODULE, {send, Argv}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Connect]) ->
  {ok, #state{connect=Connect}}.

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

handle_cast({send, Argv}, State) ->
  Connect = State#state.connect,
  {ok, Channel} = amqp_connection:open_channel(Connect),
  amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"direct_logs">>,
    type = <<"direct">>}),
  {Severity, Message} =
    case Argv of
      [S | Msg] ->
        {list_to_binary(S), list_to_binary(string:join(Msg, " "))}
    end,
  amqp_channel:cast(Channel,
    #'basic.publish'{
      exchange = <<"direct_logs">>,
      routing_key = Severity},
    #amqp_msg{payload = Message}),
  {noreply, State};

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
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
terminate(_Reason, State) ->
  Connect = State#state.connect,
  {ok, Channel} = amqp_connection:open_channel(Connect),
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connect),
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



