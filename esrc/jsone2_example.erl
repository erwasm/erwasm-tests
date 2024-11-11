-module(jsone2_example).

-export([
 encode/1,
 example/1,
 example/2,
 quote/1
]).


encode(N) ->
  { ok, Data } = jsone_encode:encode(N),
  Data.

data(N) ->
  case N of
   1 -> [<<"String 1">>, <<"Another one">>, <<"One more">>]
  end.

example(N) ->
  { ok, Data } = jsone_encode:encode(data(N)),
  Data.


example(N, InData) ->
  Input = case N of
    1 -> [<<"String X">>, InData, <<"Tail">>];
    2 -> [<<"N1">>, 2, InData, <<"TailN">>];
    3 -> [InData]
  end,
  { ok, Data } = jsone_encode:encode(Input),
  Data.


% move this to bytematch test file
quote(Code) ->
  <<$", Code:16, $">>.
