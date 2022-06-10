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
-export([start/3,
         my_group/0, my_id/0, my_clientid/0, conf_prefix/0]).

%% behavior callbacks:
-export([]).

%% internal exports:
-export([entrypoint/3, loop/1]).

-export_type([]).

-include("emqttb_internal.hrl").

-define(MY_ID, emqttb_my_id).

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

-spec my_id() -> integer().
my_id() ->
  get(?MY_ID).

-spec conf_prefix() -> lee:key().
conf_prefix() ->
  persistent_term:get(?GROUP_CONF_PREFIX(group_leader())).

-spec my_clientid() -> binary().
my_clientid() ->
  Group = my_group(),
  ID = my_id(),
  Pattern = ?CFG(conf_prefix() ++ [clientid]),
  Id0 = binary:replace(Pattern, <<"%n">>, integer_to_binary(ID), [global]),
  Id1 = binary:replace(Id0, <<"%g">>, atom_to_binary(Group), [global]),
  Id1.

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
    put(?MY_ID, Number),
    %% Note: everything up to this point must be lightweight, since
    %% group leader relies on the counter to stop scaling. If the
    %% above part takes too long, it will overshoot by large margin.
    emqttb_metrics:counter_inc(?GROUP_N_WORKERS(my_group()), 1),
    loop(Behavior)
  catch
    EC:Err:Stack ->
      emqttb_metrics:counter_dec(?GROUP_N_WORKERS(my_group()), 1),
      logger:error("[~p:~p] ~p:~p:~p", [my_group(), Number, EC, Err, Stack])
  end.

loop(Behavior) ->
  ClientId = my_clientid(),
  receive after infinity -> ok end.

%%================================================================================
%% Internal functions
%%================================================================================
