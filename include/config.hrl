-define(LOG_ERROR(Format, Data),
  lager:log(error, [], "~p:~p(): " ++ Format ++ "~n~n", [?MODULE, ?LINE] ++ Data)).
-define(LOG_WARNING(Format, Data),
  lager:log(warning, [], "~p:~p(): " ++ Format ++ "~n~n", [?MODULE, ?LINE] ++ Data)).
-define(LOG_INFO(Format, Data),
  lager:log(info, [], "~p.erl:~p: " ++ Format ++ "~n~n", [?MODULE, ?LINE] ++ Data)).
