#!/usr/bin/escript

main(_) ->
  Files = case os:getenv("APPS") of
            false -> [];
            F -> re:split(F, " ")
          end,
  Opts = main(Files, []),
  io:format("~p.~n", [Opts]).

main([], Opts) ->
  lists:reverse(Opts);
main([File | Rest], Opts) ->
  {ok, [{application, App, PropList}]} = file:consult(File),
  case proplists:get_value(env, PropList, []) of
    [] -> main(Rest, Opts);
    Env -> main(Rest, [{App, Env} | Opts])
  end.
