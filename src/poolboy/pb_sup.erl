-module(pb_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  ChildSpecs = [pool_spec()],
  {ok, {{one_for_one, 1000, 3600}, ChildSpecs}}.

pool_spec() ->
  {ok, List}  = application:get_env(webserver, pools),
  Name        = pool1,
  PoolArgs    = proplists:get_value(Name, List),
  poolboy:child_spec(Name, PoolArgs, []).