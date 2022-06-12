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
-export([my_group/0, my_id/0, my_clientid/0, cfg/1, connect/2]).

%% behavior callbacks:
-export([]).

%% internal exports:
-export([entrypoint/3, loop/1, start/3]).

-export_type([]).

-include("emqttb_internal.hrl").

-define(MY_ID, emqttb_my_id).
-define(MY_CLIENT, emqttb_my_client).

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

-spec connect([emqtt:option()], [gen_tcp:option()]) -> gen_statem:start_ret().
connect(CustomOptions, CustomTcpOptions) ->
  Username = cfg([client, username]),
  Password = cfg([client, password]),
  Options = [ {username,   Username} || Username =/= undefined]
         ++ [ {password,   Password} || Password =/= undefined]
         ++ [ {clientid,   my_clientid()}
            , {hosts,      broker_hosts()}
            , {port,       cfg([broker, port])}
            , {proto_ver,  cfg([connection, proto_ver])}
            , {low_mem,    cfg([lowmem])}
            , {tcp_opts,   tcp_opts(CustomTcpOptions)}
            | CustomOptions
            ],
  {ok, Client} = emqtt:start_link(Options),
  ConnectFun = connect_fun(),
  {ok, _Properties} =  emqtt:ConnectFun(Client),
  {ok, Client}.

-spec tcp_opts([gen_tcp:option()]) -> [gen_tcp:option()].
tcp_opts(CustomOptions) ->
  IfAddrs = cfg([ifaddr]),
  IfAddr  = lists:nth(my_id() rem length(IfAddrs) + 1, IfAddrs),
  [ {ifaddr, IfAddr}
  | CustomOptions
  ].

%% @doc Get configuration
-spec cfg(lee:key()) -> term().
cfg(Key) ->
  ConfId = ?GROUP_CONF_ID(group_leader()),
  ?CFG([groups, ConfId | Key]).

-spec my_clientid() -> binary().
my_clientid() ->
  Group = my_group(),
  ID = my_id(),
  Pattern = cfg([client, clientid]),
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
  %% We need to trap exits to make sure the counter is decremented in
  %% the end.
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
    {ok, Client} = connect([], []),
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
    case cfg([connection, transport]) of
        ws ->
            ws_connect;
        quic ->
            quic_connect;
        sock ->
            connect
    end.

-spec broker_hosts() -> [{string(), inet:port_number()}].
broker_hosts() ->
  lists:map(
    fun({Host, Port}) -> {Host, Port};
       (Host)         -> {Host, cfg([broker, port])}
    end,
    cfg([broker, hosts])).
