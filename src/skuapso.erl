-module(skuapso).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([
  start/0,
  start/2,
  stop/1]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("logger/include/log.hrl").
%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start() ->
  application:start(?MODULE).

start(_StartType, StartArgs) ->
  start_link(StartArgs).

start_link(StartArgs) ->
  ok = crypto:start(),
  ok = logger:start(),
  ok = hooks:start(),
  Reply = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  Modules = misc:get_env(?MODULE, modules, StartArgs),
  lists:map(fun(X) ->
        notice("starting ~w", [X]),
        ok = X:start()
    end, Modules),
  Reply.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init([]) ->
  {ok,
    {
      {one_for_one, 5, 10},
      [
      ]
    }
  }.
