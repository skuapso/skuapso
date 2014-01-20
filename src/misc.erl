-module (misc).
-compile({no_auto_import,[binary_to_float/1]}).

-export (
  [
    bhi/1,
    ihb/1,
    list_to_string/1,
    string_to_hex/1,
    binary_to_hex/1,
    byte_length/1,
    int_to_bool/1,
    bool_to_int/1,
    bool_to_term/2,
    get_unix_timestamp/0,
    get_unix_timestamp/1,
    ip_to_atom/1,
    is_string/1,
    get_env/2,
    get_env/3,
    get_env/4,
    datetime_to_list/2,
    is_proplist/1,
    binary_to_float/1,
    compact_list/1,
    convert_digital/1,
    interval2seconds/1,
    bits2tuples/1,
    bits2tuples/2
  ]
).

bhi (Bin) ->
  [Hex] = io_lib:format ("~.16B", [Bin]),
  list_to_integer (Hex).

ihb (I) ->
  [IStr] = io_lib:format ("~B", [I]),
  {ok, [H], []} = io_lib:fread ("~16u", IStr),
  H.

list_to_string (L) when is_list (L) ->
  binary_to_list (list_to_binary (L)).

char_to_hex (Char) ->
  if
    Char > 16#0F ->
      [Hex] = io_lib:format ("~2.16.0b", [Char]);
    true ->
      [[A, B]] = io_lib:format ("~2.16.0b", [Char]),
      Hex = A ++ [B]
  end,
  Hex.

string_to_hex (Str) ->
  lists:concat ([char_to_hex (X) || X <- Str]).

binary_to_hex (Bin) ->
  string_to_hex (binary_to_list (Bin)).

byte_length (I) when is_integer (I) ->
  byte_length (I, 0).
byte_length (0, L) ->
  L;
byte_length (I, L) ->
  byte_length (I div 256, L + 1).

int_to_bool (0) ->
  false;
int_to_bool (I) when is_integer (I) ->
  true.

bool_to_int (true) ->
  1;
bool_to_int (false) ->
  0.

bool_to_term (true, {IfTrue, _}) ->
  IfTrue;
bool_to_term (false, {_, IfFalse}) ->
  IfFalse.

get_unix_timestamp () ->
  get_unix_timestamp (now ()).
get_unix_timestamp ({MegaSecs, Secs, MicroSecs}) ->
  MegaSecs * 1000000 + Secs + MicroSecs / 1000000.

ip_to_atom ({A, B, C, D}) ->
  list_to_atom (string:join (io_lib:format ("~w~w~w~w", [A, B, C, D]), ".")).

is_string (Str) when (is_list (Str) or is_binary(Str))->
  try
    Ru =  <<208,176,45,209,143>>,
    Pattern = <<"([^0-9a-z \"\'\-", Ru/binary, "])">>,
%    Pattern = [40,91,94,48,45,57,97,45,122,1072,45,1103,95,34,32,46,93,41],
    case re:run (Str, Pattern, [unicode, caseless, no_auto_capture]) of
      nomatch ->
        true;
      _ ->
        false
    end
  catch
    _:_ ->
      false
  end;
is_string (_Str) ->
  false.

get_env(Key, Args) when is_atom(Key), is_list(Args) ->
  get_env(Key, Args, undefined).
get_env(App, Key, Args) when is_atom(App), is_atom(Key), is_list(Args) ->
  get_env(App, Key, Args, undefined);
get_env(Key, Args, Default) when is_atom(Key), is_list(Args) ->
  EnvVars = application:get_all_env(),
  proplists:get_value(Key, Args, proplists:get_value(Key, EnvVars, Default)).
get_env(App, Key, Args, Default) when is_atom(App), is_atom(Key), is_list(Args) ->
  EnvVars = application:get_all_env(App),
  proplists:get_value(Key, Args, proplists:get_value(Key, EnvVars, Default)).

datetime_to_list({Y, M, D}, {H, Mi, S}) ->
  lists:flatten(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w", [Y, M, D, H, Mi, S])).

is_proplist(L) when is_list(L) ->
  case lists:filter(fun not_proplist_element/1, L) of
    [] ->
      true;
    _ ->
      false
  end;
is_proplist(_) ->
  false.

not_proplist_element({_, _}) ->
  false;
not_proplist_element(_) ->
  true.

binary_to_float(<<S:1/integer, E:8/unsigned-big-integer, Si:23/unsigned-big-integer>>) ->
  E64 = E + 896,
  B = <<S:1/integer, E64:11/unsigned-big-integer, Si:23/unsigned-big-integer, 0:29/integer>>,
  ?MODULE:binary_to_float(B);
binary_to_float(<<F:64/signed-big-float>>) ->
  F.

compact_list([]) ->
  [];
compact_list(List) ->
%  L0 = lists:flatten(List),
  L0 = lists:delete(fun(X) ->
          case X of
            [] -> true;
            _ -> false
          end
      end, List),
  L1 = lists:map(
      fun(X) ->
          {X, lists:flatten(proplists:get_all_values(X, List))}
      end,
      proplists:get_keys(L0)),
  WithoutSet = proplists:delete(set, L1),
  case compact_list(proplists:get_value(set, L1, [])) of
    [] ->
      WithoutSet;
    Set ->
      WithoutSet ++ [{set, Set}]
  end.

convert_digital(D) ->
  convert_digital(D, 0).

convert_digital([], N) ->
  N;
convert_digital([{I, 1} | D], N) ->
  convert_digital(D, N bor (1 bsl (I - 1)));
convert_digital([{_, _} | D], N) ->
  convert_digital(D, N).

interval2seconds({{H, M, S}, D, M}) ->
  S + M * 60 + H * 3600 + D * 3600 * 24 + M * 3600 * 24 * 30.

bits2tuples(I) ->
  bits2tuples(I, 1).

bits2tuples(I, N) ->
  bits2tuples(I, N, []).

bits2tuples(0, _, T) ->
  lists:reverse(T);
bits2tuples(I, N, T) when I band 1 =:= 1 ->
  bits2tuples(I bsr 1, N + 1, [{N, 1} | T]);
bits2tuples(I, N, T) ->
  bits2tuples(I bsr 1, N + 1, T).
