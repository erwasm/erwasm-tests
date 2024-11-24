-module(fun_util).

-export([
  times_ten/1,
  times_five/1
]).

times_ten(N) when is_number(N) -> N * 10.

times_five(N) when is_number(N) -> N * 5.
