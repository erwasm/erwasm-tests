-module(datalib).

-export([query/1]).

query(N) -> 
  case N of
   1 -> outer_constant;
   V when V =< 2 ->
    % we can only get here when N is set to 2
    % but compiler is not smart enought to
    % infer that V is constat, so this forces
    % us to actually call atom to binary in runtime.
    erlang:atom_to_binary(query(V - 1), utf8);

   Other when is_atom(Other) -> erlang:atom_to_binary(Other, utf8)

  end.
