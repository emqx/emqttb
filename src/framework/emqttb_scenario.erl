%%--------------------------------------------------------------------
%% Copyright (c) 2022 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-module(emqttb_scenario).

%% API:
-export([set_stage/1, set_stage/2, stage/1, complete/1, loiter/0,
         model/0, list_enabled_scenarios/0, run/1, stop/1,
         my_scenario/0, my_scenario_module/0, my_conf/1]).

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2, terminate/2]).

%% internal exports:
-export([start_link/1]).

-include_lib("typerefl/include/types.hrl").
-include("emqttb.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type result() :: {ok | error, _} | ok.

-define(SCENARIO_GROUP_LEADER(GL), {emqttb_scenario_group_leader, GL}).

%%================================================================================
%% Behavior declaration
%%================================================================================

-callback run() -> ok.

-callback name() -> atom().

-callback model() -> lee:lee_module().

%%================================================================================
%% API functions
%%================================================================================

-spec run(module()) -> ok.
run(Module) ->
  emqttb_scenarios_sup:run(Module).

-spec stop(module()) -> ok.
stop(Module) ->
  emqttb_scenarios_sup:stop_child(Module).

%% Get module of the scenario that spawned our process
-spec my_scenario_module() -> module().
my_scenario_module() ->
  persistent_term:get(?SCENARIO_GROUP_LEADER(group_leader())).

%% Get name of the scenario that spawned our process
-spec my_scenario() -> module().
my_scenario() ->
  Mod = my_scenario_module(),
  Mod:name().

%% Get configuartion parameter of the scenario
-spec my_conf(lee:key()) -> term().
my_conf(Key) ->
  ?CFG([?SK(my_scenario_module()) | Key]).

-spec stage(emqttb:scenario()) -> emqttb:stage().
stage(Scenario) ->
  persistent_term:get({emqttb_stage, Scenario}, undefined).

-spec set_stage(emqttb:stage()) -> ok.
set_stage(Stage) ->
  Scenario = my_scenario(),
  logger:notice(asciiart:visible($#, "~p entered stage ~p", [Scenario, Stage])),
  persistent_term:put({emqttb_stage, Scenario}, Stage).

-spec set_stage(emqttb:stage(), result()) -> ok.
set_stage(Stage, ok) ->
  set_stage(Stage);
set_stage(Stage, {ok, Result}) ->
  Scenario = my_scenario(),
  logger:notice(asciiart:visible( $#, "~p completed stage ~p~nResult: ~p"
                                , [Scenario, stage(Scenario), Result]
                                )),
  set_stage(Stage);
set_stage(Stage, {error, Err}) ->
  Scenario = my_scenario(),
  logger:notice(asciiart:visible( $!, "Stage ~p of ~p failed~nError: ~p"
                                , [stage(Scenario), Scenario, Err]
                                )),
  set_stage(Stage).

-spec loiter() -> ok.
loiter() ->
  T = case my_conf([loiter]) of
        infinity -> infinity;
        Seconds  -> timer:seconds(Seconds)
      end,
  receive after T -> ok end.

-spec complete(result()) -> no_return().
complete(PrevStageResult) ->
  Scenario = my_scenario(),
  set_stage(complete, PrevStageResult),
  case PrevStageResult of
    ok         -> ok;
    {ok, _}    -> ok;
    {error, _} -> emqttb:setfail(Scenario)
  end.

-spec model() -> lee_model:lee_module().
model() ->
  application:load(?APP),
  {ok, Modules} = application:get_key(?APP, modules),
  Scenarios = [M || M <- Modules,
                    {behavior, Behaviors} <- proplists:get_value( attributes
                                                                , M:module_info()
                                                                ),
                    ?MODULE <- Behaviors],
  Model = [{M, make_model(M)} || M <- Scenarios],
  maps:from_list(Model).

list_enabled_scenarios() ->
  [I || [?SK(I)] <- ?CFG_LIST([?SK({})])].

%%================================================================================
%% External exports
%%================================================================================

-spec start_link(module()) -> {ok, pid()}.
start_link(Module) ->
  gen_server:start_link({local, Module}, ?MODULE, [Module], []).

%%================================================================================
%% gen_server callbacks
%%================================================================================

-record(s,
        { module :: module()
        }).

init([Module]) ->
  process_flag(trap_exit, true),
  logger:notice("Starting scenario ~p", [Module:name()]),
  group_leader(self(), self()),
  persistent_term:put(?SCENARIO_GROUP_LEADER(group_leader()), Module),
  {ok, #s{module = Module}, {continue, start}}.

handle_continue(start, S = #s{module = Module}) ->
  Module:run(),
  {stop, normal, S}.

handle_call(_, _, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_, S) ->
  {notrepy, S}.

terminate(_, State) ->
  persistent_term:erase(?SCENARIO_GROUP_LEADER(group_leader())).

%%================================================================================
%% Internal functions
%%================================================================================

make_model(M) ->
  Name = atom_to_list(M:name()),
  {[map, cli_action, scenario],
   #{ cli_operand => Name
    , key_elements => []
    , oneliner => "Run scenario " ++ Name
    },
   maps:merge(
     #{ loiter =>
          {[value, cli_param],
           #{ oneliner    => "Keep running scenario stages for this period of time (sec)"
            , type        => timeout()
            , default_ref => [convenience, loiter]
            , cli_operand => "loiter"
            }}
      },
     M:model())}.
