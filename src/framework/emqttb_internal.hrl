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
-ifndef(EMQTTB_INTERNAL_HRL).
-define(EMQTTB_INTERNAL_HRL, true).

-include("emqttb.hrl").

%% Persistent term keys:
-define(GROUP_LEADER_TO_GROUP_ID(GL), {emqttb_group_leader_to_group_id, GL}).
-define(GROUP_BEHAVIOR(GL), {emqttb_group_behavior, GL}).
-define(GROUP_BEHAVIOR_SHARED_STATE(GL), {emqttb_group_behavior_settings, GL}).

-define(GROUP_CONF_ID(GL), {emqttb_group_client_conf_id, GL}).

%% Metrics:
-define(GROUP_N_WORKERS(GRP), {group_n_workers, GRP}).
-define(GROUP_OP_TIME(GRP, OP), {group_op_time, {GRP, OP}}).
-define(GROUP_N_PENDING(GRP, OP), {group_n_pending, {GRP, OP}}).
-define(AUTORATE_RATE(ID), {autorate_rate, ID}).
-define(AUTORATE_CONTROL(ID, TERM), {autorate_control, {ID, TERM}}).
%%  Publisher
-define(CNT_PUB_MESSAGES(GRP), {emqttb_published_messages, GRP}).
-define(AVG_PUB_TIME, publish).
%%  Subscriber
-define(CNT_SUB_MESSAGES(GRP), {emqttb_received_messages, GRP}).
-define(AVG_SUB_TIME, subscribe).

-endif.
