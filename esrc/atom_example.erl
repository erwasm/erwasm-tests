-module(atom_example).

-export([example/1]).

example(N) -> 
  case N of
   1 -> atom_x;
   2 ->
    % compiler will cheat and put string literal right here
    erlang:atom_to_binary(atom_x, utf8);
   V when V =< 3 ->
    % we can only get here when N is set to 3
    % but compiler is not smart enought to
    % infer that V is constat, so this forces
    % us to actually call atom to binary in runtime.
    erlang:atom_to_binary(example(V - 2), utf8);
   4 -> datalib:query(2);
   5 -> datalib:query(atom_x)
  end.
