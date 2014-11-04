-module(validate).
-export([date/2]).

-define(FORMAT_SIZE_BIT, 80).

-spec date(Date::binary(), Format::binary()) -> tuple() | {error, tuple()}.
date(Date, Format) ->
  try
    date_vs_format(bit_size(Date) =:= bit_size(Format), Date, Format)
  catch
    _ : Reason -> {error, {failed, Reason}}
  end.

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
valid_dispatcher(true, Y,M,D, DateDispatcher) -> {Y,M,D, DateDispatcher};
valid_dispatcher(false, _Y,_M,_D, DateDispatcher) -> {error, {failed, dispatcher, DateDispatcher}}.

-spec is_date(binary()) -> tuple().
is_date(<<Y:32,DISAPTCHER:8,M:16,DISAPTCHER:8,D:16>>)->
  {<<Y:32>>,<<M:16>>,<<D:16>>,<<DISAPTCHER:8>>}.








