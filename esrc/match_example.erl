-module(match_example).

-export([
  matchBase/1,
  matchShort/1,
  matchLong/1,
  matchMiddle/1
]).

% this compiles into a simple is_equal check
matchBase(<<"base">>) -> base;
matchBase(_Any) -> false.

% this invloves bs_match that 
% - checking the first 31 bit, the leftover 1 bit
% - reads the last 16 bits as integer
% and then does select_val
matchShort(<<"base_x">>) -> base_x;
matchShort(<<"base_y">>) -> base_y;
matchShort(_Any) -> false.

% this invloves bs_match that 
% - checking the first 31 bit, the leftover 1 bit
% - reads the last 16 bits as integer
% then select val happens
% at the end bs_match_string check remaining 72 bits
matchLong(<<"base_1234567890">>) -> base_one;
matchLong(<<"base_0000000001">>) -> base_zero;
matchLong(_Any) -> false.

matchMiddle(<<"base_", Basic:1, Code:7, "_1234567890">>) ->
  case Basic of
    0 -> <<"ok:", Code:8>>;
    1 -> <<"nop">>
  end;

matchMiddle(_Any) -> false.

