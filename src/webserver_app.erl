-module(webserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Port = 5555,
  {ok, _} = ranch:start_listener(tcp_echo, 100,
    ranch_tcp, [{port, Port}],
    rp, []
  ),
  webserver_sup:start_link().

stop(_State) ->
    ok.
