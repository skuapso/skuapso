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
    interval2seconds/1,
    bits2map/2,
    bits2map/3,
    map2bits/1,
    update_path/3
  ]
).
-export([to_json/1]).

-include_lib("logger/include/log.hrl").

bhi (Bin) ->
  [Hex] = io_lib:format ("~.16B", [Bin]),
  list_to_integer (Hex).

ihb (I) ->
  [IStr] = io_lib:format ("~B", [I]),
  {ok, [H], []} = io_lib:fread ("~16u", IStr),
  H.

list_to_string (L) when is_list (L) ->
  binary_to_list (list_to_binary (L)).

char_to_hex (Char) when Char > 16#0f ->
  [Hex] = io_lib:format ("~2.16.0b", [Char]),
  Hex;
char_to_hex (Char) ->
  [[A, B]] = io_lib:format ("~2.16.0b", [Char]),
  A ++ [B].

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

interval2seconds({{H, M, S}, D, M}) ->
  S + M * 60 + H * 3600 + D * 3600 * 24 + M * 3600 * 24 * 30.

bits2map(N, I) -> bits2map(N, I, 0, #{}).
bits2map(N, I, T) -> bits2map(N, I, T, #{}).
bits2map(N, I, T, M) when N > 0 ->
  bits2map(N - 1, I bsr 1, T + 1, maps:put(T + 1, I band 1, M));
bits2map(_, _, _, M) -> M.

map2bits(M) -> list2bits(maps:to_list(M), 0).

list2bits([{N, V} | T], I) when V =:= 1 ->
  list2bits(T, I bor (1 bsl (N - 1)));
list2bits([_ | T], I) ->
  list2bits(T, I);
list2bits([], I) -> I.

update_path([Key | []], Value, Map) when is_map(Map) ->
  maps:put(Key, Value, Map);
update_path([Key | Path], Value, Map) when is_map(Map) ->
  maps:put(Key, update_path(Path, Value, maps:get(Key, Map, #{})), Map);
update_path(Path, Value, _) ->
  update_path(Path, Value, #{}).

to_json(L) ->
  L1 = pre_json(L),
  case catch jsxn:encode(L1) of
    {'EXIT', {badarg, _}} -> '_warning'("can't transform to json: ~w", [L1]), <<"{}">>;
    {'EXIT', Reason} -> '_warning'("jsx failed transform ~w: ~w", [L1, Reason]), <<"{}">>;
    E -> E
  end.

pre_json(Map) when is_map(Map) ->
  maps:map(
    fun(_, V) when is_map(V) -> pre_json(V);
       (LL, {G, M}) when LL =:= latitude; LL =:= longitude ->
        #{d => G, m => M};
       (_, V) -> V
    end,
    Map);
pre_json(List) when is_list(List) ->
  [pre_json(Map) || Map <- List].
