-module(fun_example).

-export([
  example/1,
  one_up/1
]).

% this one is exported
one_up(N) when is_number(N) -> N + 1.

% not exported
two_up(N) when is_number(N) -> N + 2.


reverse(In) -> reverse(In, []).
reverse([], Out) -> Out;
reverse([Head | Tail], Out) -> reverse(Tail, [Head | Out]).


lists_map(Fn, In) when is_function(Fn) -> lists_map(Fn, In, []).
lists_map(Fn, [Head | In], Out) -> lists_map(Fn, In, [Fn(Head) | Out]);
lists_map(_Fn, [], Out) -> reverse(Out).

example(N) ->
  case N of
    1 -> [1,2,3];
    Number when Number < 3 -> lists:map((fun fun_example:one_up/1), example(Number - 1));
    Number when Number < 4 -> lists:map(fun (Arg) -> two_up(Arg) end, example(Number - 2));

    Number when Number < 5 -> lists_map((fun fun_example:one_up/1), example(Number - 3));
    Number when Number < 6 -> lists_map(fun (Arg) -> two_up(Arg) end, example(Number - 4));
    Number when Number < 7 -> reverse(example(Number - 5))

  end.
