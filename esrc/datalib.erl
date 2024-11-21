-module(datalib).

-export([query/2]).

query(N, Opt) ->
  case N of
   1 -> outer_constant;
   V when V =< 2 ->
    % we can only get here when N is set to 2
    % but compiler is not smart enought to
    % infer that V is constat, so this forces
    % us to actually call atom to binary in runtime.
    OtherAtom = query(V - 1, Opt),
    % going into recursion to serialize is not necessary,
    % but nice touch anyway
    query(OtherAtom, Opt);

   Other when is_atom(Other) -> erlang:atom_to_binary(Other, Opt)

  end.
