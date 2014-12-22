-module (http).

-export ([parse/1]).

-compile (export_all).

-include_lib("logger/include/log.hrl").

parse (Data) when is_binary (Data) ->
  '_debug' ("parsing <<\"~s\">>", [Data]),
  A = try
    {ok, Packet, Rest} = erlang:decode_packet (http, Data, []),
    {ok, parse (Packet, Rest)}
  catch
    error:{badmatch, {more, _}}  ->
      '_info' ("incomplete package"),
      {error, incomplete};
    throw:incomplete ->
      {error, incomplete};
    A1:A2 ->
      '_err' ("~w : ~w", [A1, A2]),
      {A1, A2}
  end,
  '_debug' ("answer is ~w", [A]),
  A.

parse ({http_request, Method, {abs_path, URI}, {Maj, Min}}, Header) ->
  '_trace' ("http request"),
  Version = Maj + Min / 10,
  [{type, request}, {method, Method}, {uri, URI}, {version, Version}] ++ parse_header (Header);
parse ({http_response, {Maj, Min}, Answer, StrAnswer}, Header) ->
  '_trace' ("http response"),
  Version = Maj + Min / 10,
  [{type, response}, {version, Version}, {answer, Answer}, {str_answer, StrAnswer}] ++ parse_header (Header).

parse_header (<<>>) ->
  [];
parse_header (Header) ->
  '_debug' ("parsing header <<\"~s\">>", [Header]),
  {Item, Rest} = store (erlang:decode_packet (httph_bin, Header, [])),
  Item ++ parse_header (Rest).

store ({more, _}) ->
  throw (incomplete);
store ({ok, {http_header, _Length, Key, _Resolver, Value}, Rest}) when is_binary (Key) ->
  store ({list_to_atom (string:to_lower (binary_to_list (Key))), Value}, Rest);
store ({ok, {http_header, _Length, Key, _Resolver, Value}, Rest}) when is_atom (Key) ->
  store ({list_to_atom (string:to_lower (atom_to_list (Key))), Value}, Rest);
store ({ok, http_eoh, Data}) ->
  store ({data, Data}, <<>>);
store (Else) ->
  '_err' ("store unknown data: ~w", [Else]),
  {[], <<>>}.

store ({Key, Value}, Rest)
when (Key =:= 'accept')
or (Key =:= 'accept-language')
or (Key =:= 'accept-charset')
->
  NewValue = re:split (binary_to_list (Value), ",", [{return, list}]),
  {[{Key, NewValue}], Rest};

store ({Key, Value}, Rest)
when (Key =:= 'accept-encoding')
or (Key =:= 'connection')
->
  NewValue = re:split (binary_to_list (Value), ", ", [{return, list}]),
  {[{Key, NewValue}], Rest};

store ({Key , Value}, Rest) when Key == 'sec-websocket-version' ->
  Values = list_to_integer (binary_to_list (Value)),
  {[{Key, Values}], Rest};
store ({Key, Value}, Rest) ->
  NewValue = binary_to_list (Value),
  {[{Key, NewValue}], Rest}.
