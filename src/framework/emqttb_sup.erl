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
  SupFlags = #{ strategy      => one_for_one
              , intensity     => 0
              , period        => 1
              , auto_shutdown => any_significant
              },
  ChildSpecs = [ metrics()
               , scenario_sup()
               , sup(emqttb_autorate_sup)
               , sup(emqttb_group_sup)
               , sup(emqttb_misc_sup) %% Allows restarts
               ],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions

metrics() ->
  #{ id       => emqttb_metrics
   , start    => {emqttb_metrics, start_link, []}
   , type     => worker
   , shutdown => 1000
   }.

scenario_sup() ->
  #{ id          => emqttb_scenario_sup
   , start       => {emqttb_scenario_sup, start_link, []}
   , type        => supervisor
   , shutdown    => infinity
   , significant => true
   , restart     => transient
   }.

sup(Module) ->
  #{ id          => Module
   , start       => {Module, start_link, []}
   , type        => supervisor
   , shutdown    => infinity
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
