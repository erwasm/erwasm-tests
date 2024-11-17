-module(trycatch_example).

-export([
  match_fail/1,
  catch_fail/1,
  catch_some/1
]).


match_fail(N) ->
  case N of
    N when is_number(N) -> 
      N = erlang:length([]);
    Name when is_atom(Name) ->
      apply(?MODULE, Name, []);
    _Other ->
      throw(go_away)
  end.

catch_fail(N) ->
  try match_fail(N) of
    0 -> true
  catch
    error:{badmatch, _Arg} -> 
%      erlang:display(Arg),
      false;
    _Exception:_Reason -> 
%      erlang:display(Exception),
%      erlang:display(Reason),
      weird
  end.

catch_some(N) ->
  try match_fail(N) of
    0 -> true
  catch
    error:{badmatch, _Arg} -> 
%      erlang:display(Arg),
      false
  end.
