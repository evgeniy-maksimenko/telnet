-module(validate).
-export([date/3]).
-compile([export_all]).

-spec date(Date::binary(), Format::binary(), Remark::boolean()) -> boolean() | binary() .
date(Date, Format, Remark) when (is_binary(Date) and is_binary(Format) and is_boolean(Remark)) ->
  FortmatMatch = binary:matches(Format, [<<"YYYY">>,<<"YY">>,<<"yyyy">>,<<"yy">>,<<"MM">>,<<"mm">>, <<"DD">>,<<"dd">>]),
  date_vs_format(FortmatMatch, Format, Date, Remark);
date(_Date,_Format,_) -> false.

-spec date_vs_format(list(), Format::binary(), Date::binary(), Remark::boolean()) -> boolean() | binary() .
date_vs_format([{0,2},{3,2},{6,2}], <<_F1:16,F2:8,_F3:16,F4:8,_F5:16>>, <<D1:16,D2:8,D3:16,D4:8,D5:16>>, Remark) ->
  is_remark(Remark, [<<D1:16>>,<<F2:8>>,<<D3:16>>,<<F4:8>>,<<D5:16>>,D2,D4]);
date_vs_format([{0,2},{3,2},{6,4}],<<_F1:16,F2:8,_F3:16,F4:8,_F5:32>>, <<D1:16,D2:8,D3:16,D4:8,D5:32>>, Remark) ->
  is_remark(Remark, [<<D1:16>>,<<F2:8>>,<<D3:16>>,<<F4:8>>,<<D5:32>>,D2,D4]);
date_vs_format([{0,2},{3,4},{8,2}],<<_F1:16,F2:8,_F3:32,F4:8,_F5:16>>, <<D1:16,D2:8,D3:32,D4:8,D5:16>>, Remark) ->
  is_remark(Remark, [<<D1:16>>,<<F2:8>>,<<D3:32>>,<<F4:8>>,<<D5:16>>,D2,D4]);
date_vs_format([{0,4},{5,2},{8,2}], <<_F1:32,F2:8,_F3:16,F4:8,_F5:16>>, <<D1:32,D2:8,D3:16,D4:8,D5:16>>, Remark) ->
  is_remark(Remark, [<<D1:32>>,<<F2:8>>,<<D3:16>>,<<F4:8>>,<<D5:16>>,D2,D4]);
date_vs_format(_,_,_,_) -> false.

-spec is_match(boolean())-> boolean().
is_match(true)  -> true;
is_match(false) -> false.

-spec is_remark(boolean(), List::list()) -> boolean() | binary().
is_remark(false, List) ->
  [_D1,F2,_D3,F4,_D5,D2,D4] = List,
  is_match((F2 == D2) and (F4 == D4));
is_remark(true, List) ->
  [D1,F2,D3,F4,D5,_D2,_D4] = List,
  list_to_binary(binary_to_list(D1) ++ binary_to_list(F2) ++ binary_to_list(D3) ++ binary_to_list(F4) ++ binary_to_list(D5)).
