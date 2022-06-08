%%%-------------------------------------------------------------------
%%%%% @doc emqttb top level supervisor.  %% @end
%%%-------------------------------------------------------------------

-module(emqttb_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("emqttb.hrl").

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{ strategy => one_for_one
              , intensity => 0
              , period => 1
              },
  ChildSpecs = [ sup(emqttb_scenario_sup, [])
               , sup(emqttb_misc_sup, []) %% Allows restarts
               ],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions

sup(Module, Args) ->
   #{ id => Module
    , start => {Module, start_link, Args}
    , type => supervisor
    , shutdown => infinity
    }.

%% cluster_discovery() ->
%%   %% Low-tech cluster discovery with a single point of failure: the
%%   %% "bootstrap" node. If the bootstrap node fails, the cluster won't
%%   %% start. But we're not aiming for five nines here.
%%   logger:notice("Connecting to the cluster...", []),
%%   cluster_discovery_loop(),
%%   timer:sleep(1000),
%%   logger:notice("Joined the cluster", []).

%% cluster_discovery_loop() ->
%%   BootstrapNode = ?CFG(bootstrap_node),
%%   case net_adm:ping(BootstrapNode) of
%%     pong ->
%%       ok;
%%     _ ->
%%       timer:sleep(1000),
%%       cluster_discovery_loop()
%%   end.
