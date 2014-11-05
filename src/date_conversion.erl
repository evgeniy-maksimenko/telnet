-module(date_conversion).
-compile([export_all]).

main() ->
  Sheet = [<<0>>,<<1>>,<<2>>,<<3>>,<<4>>,<<5>>,<<6>>,<<7>>,<<8>>,<<9>>],
  [Date, Time]  = binary:split(to_binary("2012-12-12 00:00:00"), <<" ">>),
  [H,I,S]       = binary:split(Time, <<":">>, [global]),
  ListEl        = [ <<El>> || El <- binary_to_list(Date)],
  Dispatcher    = lists:nth(5, ListEl),
  DispatcherEL  = get_dispatcher(lists:member(Dispatcher, Sheet),Dispatcher, ListEl),
  [Y,M,D]       = binary:split(Date, DispatcherEL, [global]).

to_binary(Param) when is_binary(Param) -> Param;
to_binary(Param) when is_list(Param) -> list_to_binary(Param).

get_dispatcher(false, Dispatcher, _ListEl) ->Dispatcher;
get_dispatcher(true, _Dispatcher, ListEl) ->lists:nth(3, ListEl).