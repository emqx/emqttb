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
-export([set_stage/3, set_stage/2, stage/1, complete/2, linger/1,
         model/0, list_enabled_scenarios/0, run/1, stop/1]).

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2]).

%% internal exports:
-export([start_link/1]).

-include_lib("typerefl/include/types.hrl").
-include("emqttb.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type result() :: {ok | error, _} | ok.

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
  emqttb_scenario_sup:run(Module).

-spec stop(module()) -> ok.
stop(Module) ->
  emqttb_scenario_sup:stop_child(Module).

-spec stage(emqttb:scenario()) -> emqttb:stage().
stage(Scenario) ->
  persistent_term:get({emqttb_stage, Scenario}, undefined).

-spec set_stage(emqttb:scenario(), emqttb:stage()) -> ok.
set_stage(Scenario, Stage) ->
  logger:notice(asciiart:visible($#, "~p entered stage ~p", [Scenario, Stage])),
  persistent_term:put({emqttb_stage, Scenario}, Stage).

-spec set_stage(emqttb:scenario(), emqttb:stage(), result()) -> ok.
set_stage(Scenario, Stage, ok) ->
  set_stage(Scenario, Stage);
set_stage(Scenario, Stage, {ok, Result}) ->
  logger:notice(asciiart:visible( $#, "~p completed stage ~p~nResult: ~p"
                                , [Scenario, stage(Scenario), Result]
                                )),
  set_stage(Scenario, Stage);
set_stage(Scenario, Stage, {error, Err}) ->
  logger:notice(asciiart:visible( $!, "Stage ~p of ~p failed~nError: ~p"
                                , [stage(Scenario), Scenario, Err]
                                )),
  set_stage(Scenario, Stage).

-spec linger(module()) -> ok.
linger(Module) ->
  T = case ?CFG([?SK(Module), linger]) of
        infinity -> infinity;
        T0       -> timer:seconds(T0)
      end,
  receive after T -> ok end.

-spec complete(emqttb:scenario(), result()) -> no_return().
complete(Scenario, PrevStageResult) ->
  set_stage(Scenario, complete, PrevStageResult),
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
  logger:notice("Starting scenario ~p", [Module:name()]),
  {ok, #s{module = Module}, {continue, start}}.

handle_continue(start, S = #s{module = Module}) ->
  Module:run(),
  {stop, normal, S}.

handle_call(_, _, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_, S) ->
  {notrepy, S}.

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
     #{ linger =>
          {[value, cli_param],
           #{ oneliner    => "Keep running scenario stages for this period of time (sec)"
            , type        => timeout()
            , default_ref => [convenience, linger]
            , cli_operand => "linger"
            }}
      },
     M:model())}.
