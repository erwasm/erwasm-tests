-module(jsone1_example).

-export([
 decode/1,
 test/1
]).


select_key([{K, V} | T], Q) ->
  case K of
    Q -> V;
    _ -> select_key(T, Q)
  end;

select_key([], Q) -> nil.

select_example(ExampleN) ->
  case ExampleN of
   0 -> <<"5124">>;
   1 -> <<"\"x name\"">>;
   2 -> <<"[\"x name\"]">>;
   3 -> <<"[ 335 ]">>;
   4 -> <<"[ 1, 22, 300, 1000 ]">>;
   5 -> <<"{ \"x\" : 523 }">>;
   6 -> <<"{ \"vala\" : 3, \"valb\": 20 }">>;
   7 -> <<"{ \"value_a\" : 4, \"value1234567b\": 50 }">>;
   8 -> <<"{ \"a\" : 1, \"b\": 70, \"c\": 9, \"z\": 10 }">>
  end.

decode(N) -> 
  { ok, Value, _Rest } = jsone_decode:decode(select_example(N), [
    { object_format, tuple }
  ]),
  Value.

test(N) ->
  case jsone_decode:decode(select_example(N)) of
    { ok, <<"x name">>, _Rest } ->  1;
    { ok, [<<"x name">>], _Rest } -> 2;
    { ok, [ X ], _Rest } -> X;
    { ok, [ A, B, C, D ], _Rest } -> A + B + C +D;
    { ok, { [ { <<"x">>, V } ] }, _Rest } -> V;
    { ok, { [ {<<"vala">>, A}, {<<"valb">>, B} ] }, _Rest } -> A + B;
    { ok, { [ {<<"value_a">>, A}, {<<"value1234567b">>, B} ] }, _Rest } -> A + B;
    { ok, { PropList }, _Rest } ->
       select_key(PropList, <<"a">>) + 
       select_key(PropList, <<"z">>);
    { ok, _Value, _Rest } -> 16#BAD_DEAD
  end.
