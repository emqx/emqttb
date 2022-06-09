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
-ifndef(EMQX_BENCHD_HRL).
-define(EMQX_BENCHD_HRL, true).

-define(APP, emqttb).

-define(CONF_STORE, lee_conf_store).
-define(MODEL_STORE, lee_model).

-define(MYCONF, persistent_term:get(?CONF_STORE)).
-define(MYMODEL, persistent_term:get(?MODEL_STORE)).

-define(DEFAULT_PORT, 8017).

-define(CFG(Key), lee:get( ?MYMODEL
                         , ?MYCONF
                         , Key
                         )).

-define(CFG_LIST(Key), lee:list( ?MYMODEL
                               , ?MYCONF
                               , Key
                               )).

-define(SK(SCENARIO), scenarios, SCENARIO, {}).
-define(SK, ?SK(?MODULE)).

-define(LINGER(), emqttb_scenario:linger(?MODULE)).

-define(STAGE(S, RESULT), emqttb_scenario:set_stage(?MODULE:name(), S, RESULT)).
-define(STAGE(S), emqttb_scenario:set_stage(?MODULE:name(), S)).

-define(COMPLETE(RESULT), emqttb_scenario:complete(?MODULE:name(), RESULT)).

-endif.
