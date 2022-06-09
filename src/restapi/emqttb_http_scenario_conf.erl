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
-module(emqttb_http_scenario_conf).

-export([ init/2
        , init/3
        , descr/0
        , handle_request/2
        , content_types_provided/2
        , resource_exists/2
        ]).

descr() ->
  "Get or set a configuration parameter for a scenario while it's running.".

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req , State) ->
  {[<<"GET">>, <<"PUT">>], Req, State}.

resource_exists(Req = #{bindings := #{scenario := Scenario, key := Key}}, State) ->
  Enabled = [atom_to_binary(I:name()) || I <- emqttb_scenario:list_enabled_scenarios()],
  Exists = lists:member(Scenario, Enabled),
  {Exists, Req, State}.

content_types_provided(Req, State) ->
  {[{<<"text/plain">>, handle_request}], Req, State}.

handle_request(Req = #{bindings := #{scenario := Scenario}}, State) ->
  Stage = atom_to_binary(emqttb_scenario:stage(binary_to_atom(Scenario))),
  {Stage, Req, State}.
