-module(record_example).

-export([
  example/1
]).

-record(price, { base = 5, extra = 23 }).

parse(Buffer) -> parse(#price{}, Buffer).

parse(P, <<"extra=", Code:8, ";", Rest/binary>>) -> 
  parse(P#price{ extra = Code - 16#30 }, Rest);
parse(P, <<"base=", Code:8, ";", Rest/binary>>) ->
  parse(P#price{ base = Code - 16#30 }, Rest);
parse(P, <<"">>) -> P.

example(N) ->
  case N of
    1 -> Price = #price{}, Price#price.base;
    2 -> Price = #price{}, Price#price.extra;
    Buffer when is_binary(Buffer) ->
      Price = parse(Buffer),
      Price#price.base + Price#price.extra;
    _Other -> what
  end.
