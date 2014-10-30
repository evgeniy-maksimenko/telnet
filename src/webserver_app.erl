-module(webserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  {ok, List} = application:get_env(webserver, ranch_listener),

  Port = proplists:get_value(port, List),
    {ok, _} = ranch:start_listener(tcp_echo, proplists:get_value(nb_acceptors, List),
    ranch_tcp, [{port, Port}],
    rp, []
  ),
  webserver_sup:start_link().

stop(_State) ->
    ok.
