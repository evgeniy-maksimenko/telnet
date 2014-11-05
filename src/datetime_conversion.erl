-module(datetime_conversion).
-compile([export_all]).

check_conv(Datetime, datetime, _UTC) ->
  {Y, M, D, H, I, S} = get_data(Datetime),
  {{Y,M,D},{H,I,S}};
check_conv(Datetime, unixtime, UTC) ->
  {Y, M, D, H, I, S} = get_data(Datetime),
  calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H + UTC, I, S}}).

to_binary(Param) when is_binary(Param) -> Param;
to_binary(Param) when is_list(Param) -> list_to_binary(Param).

get_dispatcher(false, Dispatcher, _ListEl) ->Dispatcher;
get_dispatcher(true, _Dispatcher, ListEl) ->lists:nth(3, ListEl).

get_data(Datetime) ->
  Sheet = [<<0>>,<<1>>,<<2>>,<<3>>,<<4>>,<<5>>,<<6>>,<<7>>,<<8>>,<<9>>],
  [Date, Time]  = binary:split(to_binary(Datetime), <<" ">>),
  [H,I,S]       = binary:split(Time, <<":">>, [global]),
  ListEl        = [ <<El>> || El <- binary_to_list(Date)],
  Dispatcher    = lists:nth(5, ListEl),
  DispatcherEL  = get_dispatcher(lists:member(Dispatcher, Sheet),Dispatcher, ListEl),
  [Y,M,D]       = binary:split(Date, DispatcherEL, [global]),
  {binary_to_integer(Y),binary_to_integer(M),binary_to_integer(D),binary_to_integer(H),binary_to_integer(I),binary_to_integer(S)}.