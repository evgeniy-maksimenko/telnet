-module(validate).
-export([date/2]).
-export([convert/2]).

-define(FORMAT_SIZE_BIT, 80).
%% =============================================================================
%% > validate:date(<<"2012-10-11">>, <<"YYYY-MM-DD">>).
%% `~!@#$%^&*()_+{}[]"';:<>,./?|\
%% YY YYYY MM dd
%% =============================================================================
-spec date(Date::binary(), Format::binary()) -> tuple() | {error, tuple()}.
date(Date, Format) when (is_binary(Date) and is_binary(Format))  ->
  try
    date_vs_format(bit_size(Date) =:= bit_size(Format), Date, Format)
  catch
    _ : Reason -> {error, {failed, Reason}}
  end;
date(Date, Format) -> {error, {failed, insert_data, Date, Format}}.

-spec date_vs_format(boolean(), Date::binary(), Format::binary()) -> tuple() | {error, tuple()}.
date_vs_format(true, Date, Format) ->
  {_,_,_,FormatDispatcher} = valid_size(bit_size(Format), Format),
  {Y,M,D,DateDispatcher}   = valid_size(bit_size(Date), Date),
  valid_dispatcher(FormatDispatcher =:= DateDispatcher, Y,M,D, DateDispatcher);
date_vs_format(true, Date, Format) -> {error,{failed, Date, Format}}.

-spec valid_size(Size::integer(), Date::binary | _) -> tuple() | {error, tuple()}.
valid_size(?FORMAT_SIZE_BIT, Date) -> is_date(Date);
valid_size(Size, _) -> {error,{failed, size, Size}}.

-spec valid_dispatcher(boolean(), Y::boolean(),M::boolean(),D::boolean(), DateDispatcher::boolean()) -> tuple() | {error, tuple()}.
valid_dispatcher(true, Y,M,D, _DateDispatcher) -> {binary_to_integer(Y),binary_to_integer(M), binary_to_integer(D)};
valid_dispatcher(false, _Y,_M,_D, DateDispatcher) -> {error, {failed, dispatcher, DateDispatcher}}.

-spec is_date(binary()) -> tuple().
is_date(<<Y:32,DISAPTCHER:8,M:16,DISAPTCHER:8,D:16>>)->
  {<<Y:32>>,<<M:16>>,<<D:16>>,<<DISAPTCHER:8>>}.

convert(Date, binary) ->
  {{Y,M,D},{H,I,S}} = Date,
  IntTolist =
    integer_to_list(Y) ++ "-" ++
    integer_to_list(M) ++ "-" ++
    integer_to_list(D) ++ " " ++
    integer_to_list(H) ++ ":" ++
    integer_to_list(I) ++ ":" ++
    integer_to_list(S) ,
  list_to_binary(IntTolist);
convert(Date, datetime) -> Date;
convert(Date, unixtime) ->
  {{Y,M,D},{H,I,S}} = Date,
  calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H, I, S}}).

