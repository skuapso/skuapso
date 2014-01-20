-module(ll).

-export([float/1, gm/1]).

float(L) when is_float(L) ->
  L;
float(L) when is_integer(L) ->
  L + 0.0;
float({G, M}) when G >= 0 ->
  G + M / 60.0;
float({G, M}) when G < 0 ->
  -ll:float({-G, M}).

gm({G, M} = GM) when is_integer(G) andalso (is_float(M) or is_integer(M)) ->
  GM;
gm(L) when is_float(L) andalso L>=0 ->
  G = trunc(L),
  M = (L - G) * 60,
  {G, M};
gm(L) when is_float(L) ->
  {G, M} = gm(-L),
  {-G, M}.
