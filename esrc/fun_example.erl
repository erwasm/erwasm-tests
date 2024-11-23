-module(fun_example).

-export([
  example/1,
  one_up/1
]).

n_up(N, Value) when is_number(N) ->
  N + Value.

% this one is exported
one_up(N) when is_number(N) -> n_up(1, N).

% not exported
two_up(N) when is_number(N) -> n_up(2, N).

to_binary(List) -> to_binary(List, <<>>).
to_binary([Head | Tail], <<Binary/binary>>) -> to_binary(Tail, <<Binary/binary, Head:8>>);
to_binary([], Binary) -> Binary.

reverse(In) -> reverse(In, []).
reverse([Head | Tail], Out) -> reverse(Tail, [Head | Out]);
reverse([], Out) -> Out.

lists_map(Fn, In) when is_function(Fn) -> lists_map(Fn, In, []);
% TODO: make it throw 'no_case' if there is not case matching.
lists_map(_Fn, _In) -> []. % may the chaos take the world

lists_map(Fn, [Head | In], Out) ->
  lists_map(Fn, In, [Fn(Head) | Out]);
lists_map(_Fn, [], Out) -> reverse(Out).

example_list(N) ->
  case N of
    1 -> [1,2,3];
%    Number when Number < 3 -> lists:map((fun fun_example_list:one_up/1), example_list(Number - 1));
%    Number when Number < 4 -> lists:map(fun (Arg) -> two_up(Arg) end, example_list(Number - 2));

%    Number when Number < 5 -> lists_map((fun fun_example_list:one_up/1), example_list(Number - 3));

    % 5
    Number when Number < 6 ->
      lists_map(
        example_list(Number + 50),
        example_list(Number - 4)
      );
    % 6
    Number when Number < 7 ->
      lists_map(
        example_list(Number + 60),
        example_list(Number - 5)
      );
    % 7
    Number when Number < 8 ->
      lists_map(
        example_list(Number + 70),
        example_list(Number - 6)
      );

    % 10
    Number when Number < 20 -> reverse(example_list(Number - 10));

    % look up function dynamically, otherwise
    % compiler will figure out it's
    % a constant and will always call
    % the same hardcoded funcion in lists_map
    Number when Number < 60 -> fun (Arg) -> one_up(Arg) end;
    Number when Number < 70 -> (fun two_up/1);
    Number when Number < 80 -> fun (Arg) -> n_up(Number - 70, Arg) end;

    Number when Number < 200 -> fun (Arg) -> n_up(Number - 100, Arg) end;

    % Number when Number < 200 -> (fun fun_example_list:one_up/1);
    _Other -> []

  end.

example(N) -> to_binary(example_list(N)).
