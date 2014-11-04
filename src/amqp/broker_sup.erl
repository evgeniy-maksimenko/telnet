-module(broker_sup).
-behaviour(supervisor).
-include_lib("amqp_client/include/amqp_client.hrl").
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
  {ok, RabbitMq}  = application:get_env(webserver, rabbitmq),

  ets:new(logs, [named_table, public, set]),

  Flags = {one_for_one, 5, 10},
  %%{ok, Connect} = amqp_connection:start(#amqp_params_network{host = proplists:get_value(host, RabbitMq)}),
  %%{ok, Channel} = amqp_connection:open_channel(Connect),

  %%BrokerWorkerer = {broker_worker, {broker_worker, start_link, [Connect, Channel]}, permanent, 10500, worker, [broker_worker]},
  %%ConsumerWorker = {consumer_worker, {consumer_worker, start_link, [Channel]}, permanent, 10500, worker, [consumer_worker]},
  {ok, { Flags , [] }}.
