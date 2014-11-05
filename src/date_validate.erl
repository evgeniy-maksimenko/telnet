%% Verifies that the value is a date pattern
%% The following pattern characters are recognized:
%%
%% Pattern |      Description
%% ----------------------------------------------------
%% dd      | Day of month 01 to 31, zero leading
%% DD      | Day of month 01 to 31, zero leading
%% mm      | Month digit 01 to 12, zero leading
%% MM      | Month digit 01 to 12, zero leading
%% yyyy    | 4 year digit
%% YYYY    | 4 year digit
%%
%% For example, to check a date binary <<"21-10-2008">>, use the following:
%% date_validate:check_date(<<"21-10-2008">>, <<"DD-MM-YYYY").

-module(date_validate).
-export([check_date/2]).

-spec check_date(Date::binary(), Format::binary()) -> boolean() .
check_date(Date, Format) when (is_binary(Date) and is_binary(Format)) ->
  date_vs_format(format_match(Format), Format, Date);
check_date(_Date,_Format) -> false.

-spec is_match(boolean())-> boolean() .
is_match(true) -> true;
is_match(false) -> false.

-spec format_match(Format::binary()) -> binary().
format_match(Format) ->
  binary:matches(Format, [<<"YYYY">>,<<"YY">>,<<"yyyy">>,<<"yy">>,<<"MM">>,<<"mm">>, <<"DD">>,<<"dd">>]).

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