-module(webserver_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-record(chat_table,{
  tid  = chat_manager
}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  C = #chat_table{},
  ets:new(C#chat_table.tid, [named_table, public, set]),

  Flags = {one_for_one, 5, 10},
  PbSup = {pb_sup, {pb_sup, start_link, []}, permanent, 10500, supervisor, [pb_sup]},
  BrokerSup = {broker_sup, {broker_sup, start_link, []}, permanent, 10500, supervisor, [broker_sup]},
  {ok, { Flags , [PbSup, BrokerSup]} }.

