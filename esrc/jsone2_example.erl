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
   1 -> [<<"String 1">>, <<"Another one">>, <<"One more">>];
   2 -> "ASCII"; % beware, this is secretly an array
   22 -> "Ґанок"; % beware, this is secretly an array
   3 -> [<<"N1">>, "ASCII"];
   32 -> [<<"N1">>, ascii];
   4 -> { [ { <<"x">>, 1 } ] };
   5 -> { [ { "x", 1 } ] } ; % this will not encode
   6 -> { [ { xatom, 1 } ] } % needs atom to string TBD
  end.

example(N) ->
  case jsone_encode:encode(data(N)) of
    { ok, Data } -> Data;
    _Else -> <<"Encoding error">>
  end.

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
