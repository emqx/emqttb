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
-ifndef(EMQX_BENCHD_HRL).
-define(EMQX_BENCHD_HRL, true).

-define(APP, emqttb).

-define(CONF_STORE, lee_conf_store).
-define(MODEL_STORE, lee_model_store).

-include_lib("lee/include/lee.hrl").

-define(MYCONF, ?lee_persistent_term_storage(?CONF_STORE)).
-define(MYMODEL, persistent_term:get(?MODEL_STORE)).

-define(CFG(Key), lee:get( ?MYMODEL
                         , ?MYCONF
                         , Key
                         )).

-define(CFG_LIST(Key), lee:list( ?MYMODEL
                               , ?MYCONF
                               , Key
                               )).

-define(SK(SCENARIO), scenarios, SCENARIO, {}).

-endif.
