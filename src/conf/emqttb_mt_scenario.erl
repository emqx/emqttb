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
-module(emqttb_mt_scenario).

-behavior(lee_metatype).

%% behavior callbacks:
-export([names/1, post_patch/5]).

-include("emqttb.hrl").

%%================================================================================
%% behavior callbacks
%%================================================================================

names(_) ->
  [scenario].

%% This will automatically start or stop scenarios when the config changes:
post_patch(scenario, _, _, _, {set, [?SK(Scenario)], _}) ->
  emqttb_scenario:run(Scenario);
post_patch(scenario, _, _, _, {rm, [?SK(Scenario)]}) ->
  emqttb_scenario:stop(Scenario).
