-module(emqttb_scenario_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1, stop_child/1]).

-export([init/1]).

-include("emqttb.hrl").

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Module) ->
  supervisor:start_child(?SERVER, #{ id => Module:name()
                                   , start => {emqttb_scenario, start_link, [Module]}
                                   , type => worker
                                   , restart => transient
                                   , significant => true
                                   , shutdown => brutal_kill
                                   }).


stop_child(Module) ->
  Id = Module:name(),
  supervisor:terminate_child(?SERVER, Id),
  supervisor:delete_child(?SERVER, Id).

init([]) ->
  SupFlags = #{ strategy => one_for_all
              , intensity => 0
              , period => 1
              , auto_shutdown => all_significant
              },
  ChildSpecs = [],
  %%cluster_discovery(),
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
