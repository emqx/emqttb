-module(emqttb_scenario_sup).

-behaviour(supervisor).

-export([start_link/0, enable_autostop/0, run/1, stop_child/1]).

-export([init/1]).

-export([start_retainer/0]).

-include("emqttb.hrl").

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

run(Module) ->
  Id = Module:name(),
  Spec = #{ id          => Id
          , start       => {emqttb_scenario, start_link, [Module]}
          , type        => worker
          , restart     => transient
          , significant => true
          , shutdown    => brutal_kill
          },
  Result = supervisor:start_child(?SERVER, Spec),
  case Result of
    {ok, _} ->
      ok;
    {error, {already_started, _}} ->
      %% Let it run...
      ok;
    {error, already_present} ->
      supervisor:restart_child(?SERVER, Id),
      ok
  end.

stop_child(Module) ->
  Id = Module:name(),
  supervisor:terminate_child(?SERVER, Id),
  supervisor:delete_child(?SERVER, Id).

%% This supervisor is configured to stop when all of its significant
%% children terminate. Initially it starts a dummy retainer process
%% that keeps it alive even after when all scenarios complete.
%%
%% To enable autostop this retainer child is terminated.
enable_autostop() ->
  logger:debug("Enabling autostop"),
  exit(whereis(emqttb_scenario_sup_retainer), shutdown).

init([]) ->
  SupFlags = #{ strategy => one_for_all
              , intensity => 0
              , period => 1
              , auto_shutdown => all_significant
              },
  ChildSpecs = [retainer()],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions

retainer() ->
  #{ id => '$retainer'
   , type => worker
   , restart => transient
   , significant => true
   , start => {?MODULE, start_retainer, []}
   , shutdown => brutal_kill
   }.

%% This child does nothing but sits there and prevents the supervisor
%% from stopping.
-dialyzer({nowarn_function, start_retainer/0}).
start_retainer() ->
  {ok, spawn_link(fun() ->
                      register(emqttb_scenario_sup_retainer, self()),
                      receive after infinity -> ok end
                  end)}.
