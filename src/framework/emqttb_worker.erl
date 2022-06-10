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
-module(emqttb_worker).

%% API:
-export([start/3, my_group/0]).

%% behavior callbacks:
-export([]).

%% internal exports:
-export([entrypoint/3, loop/2]).

-export_type([]).

-include("emqttb_internal.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API funcions
%%================================================================================

-spec start(module(), pid(), non_neg_integer()) -> {pid(), reference()}.
start(Behavior, Group, Number) ->
  spawn_monitor(?MODULE, entrypoint, [Behavior, Group, Number]).

-spec my_group() -> emqttb:group().
my_group() ->
  persistent_term:get(?GROUP_LEADER_TO_GROUP_ID(group_leader())).

%%================================================================================
%% behavior callbacks
%%================================================================================

%%================================================================================
%% Internal exports
%%================================================================================

entrypoint(Behavior, Group, Number) ->
  try
    process_flag(trap_exit, true),
    group_leader(Group, self()),
    emqttb_metrics:counter_inc(?GROUP_N_WORKERS(my_group()), 1),
    loop(Behavior, Number)
  catch
    EC:Err:Stack ->
      emqttb_metrics:counter_dec(?GROUP_N_WORKERS(my_group()), 1),
      logger:debug("[~p:~p] ~p:~p:~p", [my_group(), Number, EC, Err, Stack])
  end.

loop(Behavior, Number) ->
  receive after infinity -> ok end.

%%================================================================================
%% Internal functions
%%================================================================================
