%%--------------------------------------------------------------------
%% Copyright (c) 2022-2023 EMQ Technologies Co., Ltd. All Rights Reserved.
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
         my_scenario/0, my_conf_key/1, my_conf/1, module/1, name/1,
         info/0]).

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2, terminate/2]).

%% internal exports:
-export([start_link/1, run_scenarios/0]).

-export_type([result/0]).

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

-callback model() -> lee:lee_module().

%%================================================================================
%% API functions
%%================================================================================

-spec run(emqttb:scenario()) -> ok.
run(Name) ->
  emqttb_scenarios_sup:run(Name).

-spec stop(module()) -> ok.
stop(Module) ->
  emqttb_scenarios_sup:stop_child(Module).

%% Get name of the scenario that spawned our process
-spec my_scenario() -> module().
my_scenario() ->
  persistent_term:get(?SCENARIO_GROUP_LEADER(group_leader())).

-spec my_conf_key(lee:key()) -> lee:key().
my_conf_key(Key) ->
  [?SK(my_scenario()) | Key].

%% Get configuartion parameter of the scenario
-spec my_conf(lee:key()) -> term().
my_conf(Key) ->
  ?CFG(my_conf_key(Key)).

-spec stage(emqttb:scenario()) -> emqttb:stage().
stage(Scenario) ->
  persistent_term:get({emqttb_stage, Scenario}, undefined).

-spec set_stage(emqttb:stage()) -> ok.
set_stage(Stage) ->
  Scenario = my_scenario(),
  Txt = lists:flatten(io_lib:format("~p entered stage ~p", [Scenario, Stage])),
  logger:notice(asciiart:visible($#, Txt, [])),
  emqttb_grafana:annotate(Txt, [Scenario]),
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
  T = my_conf([loiter]),
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
  Scenarios = all_scenario_modules(),
  Model = [{name(M), make_model(M)} || M <- Scenarios],
  maps:from_list(Model).

list_enabled_scenarios() ->
  [I || [?SK(I)] <- ?CFG_LIST([?SK({})])].

%% @doc Convert name to module
-spec module(emqttb:scenario()) -> module().
module(Id) ->
  list_to_existing_atom("emqttb_scenario_" ++ atom_to_list(Id)).

%% @doc Convert module to scenario name
-spec name(module()) -> emqttb:scenario().
name(Module) ->
  _ = Module:module_info(),
  "emqttb_scenario_" ++ Name = atom_to_list(Module),
  list_to_atom(Name).

-spec info() -> [map()].
info() ->
  lists:map(
    fun(Module) ->
        Name = name(Module),
        #{ id => Name
         , enabled => lists:member(Name, list_enabled_scenarios())
         }
    end,
    all_scenario_modules()).

%%================================================================================
%% External exports
%%================================================================================

-spec start_link(module()) -> {ok, pid()}.
start_link(Module) ->
  gen_server:start_link({local, Module}, ?MODULE, [Module], []).

-spec run_scenarios() -> ok.
run_scenarios() ->
  lists:foreach(
    fun([scenarios, Name]) ->
        case lee:list(?MYCONF, [?SK(Name)]) of
          [] -> ok;
          [_] -> emqttb_scenario:run(Name)
        end
    end,
    lee_model:get_metatype_index(scenario, ?MYMODEL)).

%%================================================================================
%% gen_server callbacks
%%================================================================================

-record(s,
        { module :: module()
        }).

init([Module]) ->
  process_flag(trap_exit, true),
  Name = name(Module),
  logger:notice("Starting scenario ~p", [Name]),
  group_leader(self(), self()),
  persistent_term:put(?SCENARIO_GROUP_LEADER(group_leader()), Name),
  {ok, #s{module = Module}, {continue, start}}.

handle_continue(start, S = #s{module = Module}) ->
  Module:run(),
  {stop, normal, S}.

handle_call(_, _, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_, S) ->
  {notrepy, S}.

terminate(_, _State) ->
  persistent_term:erase(?SCENARIO_GROUP_LEADER(group_leader())).

%%================================================================================
%% Internal functions
%%================================================================================

make_model(Module) ->
  Name = name(Module),
  {[map, cli_action, scenario],
   #{ cli_operand => atom_to_list(Name)
    , key_elements => []
    , oneliner => lists:concat(["Run scenario ", Name])
    },
   maps:merge(
     #{ loiter =>
          {[value, cli_param],
           #{ oneliner    => "Keep running scenario stages for this period of time (sec)"
            , type        => emqttb:wait_time()
            , default_ref => [convenience, loiter]
            , cli_operand => "loiter"
            }}
      },
     Module:model())}.

-spec all_scenario_modules() -> [module()].
all_scenario_modules() ->
  {ok, Modules} = application:get_key(?APP, modules),
  [M || M <- Modules,
        {behavior, Behaviors} <- proplists:get_value( attributes
                                                    , M:module_info()
                                                    ),
        ?MODULE <- Behaviors].
