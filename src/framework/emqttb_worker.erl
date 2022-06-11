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

%% Worker API:
-export([my_group/0, my_id/0, my_clientid/0, cfg/1, connect/1]).

%% behavior callbacks:
-export([]).

%% internal exports:
-export([entrypoint/3, loop/1, start/3]).

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

%% @doc Get configuration
-spec cfg(lee:key()) -> term().
cfg(Key) ->
  ConfId = ?GROUP_CONF_ID(group_leader()),
  ?CFG([groups, ConfId | Key]).

connect(Opts) ->
  ok.

-spec my_clientid() -> binary().
my_clientid() ->
  Group = my_group(),
  ID = my_id(),
  Pattern = cfg([clientid]),
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
  process_flag(trap_exit, true),
  group_leader(Group, self()),
  put(?MY_ID, Number),
  %% Note: everything up to this point must be lightweight, since
  %% group leader relies on the counter to stop scaling. If the
  %% above part takes too long, it will overshoot by large margin.
  emqttb_metrics:counter_inc(?GROUP_N_WORKERS(my_group()), 1),
  loop(Behavior).

loop(Behavior) ->
  try
    ClientId = my_clientid(),
    receive after 10000 -> ok end
  catch
    EC:Err:Stack ->
      logger:error("[~p:~p] ~p:~p:~p", [my_group(), my_id(), EC, Err, Stack])
  after
    emqttb_metrics:counter_dec(?GROUP_N_WORKERS(my_group()), 1)
  end.

%%================================================================================
%% Internal functions
%%================================================================================

%% Connection

-spec connect_fun() -> FunName :: atom().
connect_fun()->
    case cfg([transport]) of
        ws ->
            ws_connect;
        quic ->
            quic_connect;
        mqtt ->
            connect
    end.
