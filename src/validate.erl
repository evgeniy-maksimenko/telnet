-module(validate).
-export([date/2]).
-compile([export_all]).

-spec date(Date::binary(), Format::binary()) -> boolean() .
date(Date, Format) when (is_binary(Date) and is_binary(Format)) ->
  FortmatMatch = binary:matches(Format, [<<"YYYY">>,<<"YY">>,<<"yyyy">>,<<"yy">>,<<"MM">>,<<"mm">>, <<"DD">>,<<"dd">>]),
  date_vs_format(FortmatMatch, Format, Date);
date(_Date,_Format) -> false.

-spec date_vs_format(list(), Format::binary(), Date::binary()) -> boolean().
date_vs_format([{0,2},{3,2},{6,2}], <<_F1:16,F2:8,_F3:16,F4:8,_F5:16>>, <<_D1:16,D2:8,_D3:16,D4:8,_D5:16>>) ->
  is_match((F2 == D2) and (F4 == D4));
date_vs_format([{0,2},{3,2},{6,4}],<<_F1:16,F2:8,_F3:16,F4:8,_F5:32>>, <<_D1:16,D2:8,_D3:16,D4:8,_D5:32>>) ->
  is_match((F2 == D2) and (F4 == D4));
date_vs_format([{0,2},{3,4},{8,2}],<<_F1:16,F2:8,_F3:32,F4:8,_F5:16>>, <<_D1:16,D2:8,_D3:32,D4:8,_D5:16>>) ->
  is_match((F2 == D2) and (F4 == D4));
date_vs_format([{0,4},{5,2},{8,2}], <<_F1:32,F2:8,_F3:16,F4:8,_F5:16>>, <<_D1:32,D2:8,_D3:16,D4:8,_D5:16>>) ->
  is_match((F2 == D2) and (F4 == D4));
date_vs_format(_,_,_) -> false.

-spec is_match(boolean())-> boolean() .
is_match(true)  -> true;
is_match(false) -> false.