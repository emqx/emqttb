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
-export([my_group/0, my_id/0, my_clientid/0, my_cfg/1, connect/2]).

%% internal exports:
-export([entrypoint/3, loop/1, start/3, create_settings/3]).

-export_type([]).

-include("emqttb_internal.hrl").
-include_lib("snabbkaffe/include/trace.hrl").

-define(MY_ID, emqttb_my_id).
-define(MY_CLIENT, emqttb_my_client).

%%================================================================================
%% Behavior callbacks
%%================================================================================

%% Called once, during initializtion of the group. This callback
%% allows to create a static, shared behavior state.
-callback create_settings(emqttb:group(), _BehaviorSettings) -> _Shared.

%% Called when the worker is started. For example, you can start an
%% MQTT client here. Avoid keeping static and shared terms in the
%% state, move it to `create_settings' instead.
-callback init(map()) -> _State.

%% Self-explanotary. Called every time when worker receives a message
%% that is not an <code>'EXIT'</code>
-callback handle_message(State, _Shared, _Msg) -> {ok, State} | {exit, State}.

-callback terminate(_Shared, _State) -> _.

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API funcions
%%================================================================================

-spec start(module(), pid(), non_neg_integer()) -> pid().
start(Behavior, Group, Number) ->
  Options = [],
  spawn_opt(?MODULE, entrypoint, [Behavior, Group, Number], Options).

-spec create_settings(module(), emqttb:group(), term()) -> term().
create_settings(Module, GroupID, Opts) ->
  Module:create_settings(GroupID, Opts).

-spec my_group() -> emqttb:group().
my_group() ->
  persistent_term:get(?GROUP_LEADER_TO_GROUP_ID(group_leader())).

-spec my_id() -> integer().
my_id() ->
  get(?MY_ID).

-spec connect([emqtt:option()], [gen_tcp:option()]) -> gen_statem:start_ret().
connect(CustomOptions, CustomTcpOptions) ->
  Username = my_cfg([client, username]),
  Password = my_cfg([client, password]),
  Options = [ {username,   Username} || Username =/= undefined]
         ++ [ {password,   Password} || Password =/= undefined]
         ++ [ {clientid,   my_clientid()}
            , {hosts,      broker_hosts()}
            , {port,       my_cfg([broker, port])}
            , {proto_ver,  my_cfg([connection, proto_ver])}
            , {low_mem,    my_cfg([lowmem])}
            , {tcp_opts,   tcp_opts(CustomTcpOptions)}
            ],
  {ok, Client} = emqtt:start_link(CustomOptions ++ Options),
  ConnectFun = connect_fun(),
  {ok, _Properties} =  emqtt:ConnectFun(Client),
  {ok, Client}.

-spec tcp_opts([gen_tcp:option()]) -> [gen_tcp:option()].
tcp_opts(CustomOptions) ->
  [ {ifaddr, ifaddr()}
  | CustomOptions
  ].

ifaddr() ->
  IfAddrs = my_cfg([net, ifaddr]),
  lists:nth(my_id() rem length(IfAddrs) + 1, IfAddrs).

%% @doc Get group-specific configuration (as opposed to global)
-spec my_cfg(lee:key()) -> term().
my_cfg(Key) ->
  ConfId = ?GROUP_CONF_ID(group_leader()),
  ?CFG([groups, ConfId | Key]).

-spec my_clientid() -> binary().
my_clientid() ->
  Group = my_group(),
  ID = my_id(),
  Pattern = my_cfg([client, clientid]),
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
  ?tp(emqttb_worker_start, #{gl => Group, number => Number}),
  %% Note: everything up to this point must be lightweight, since
  %% group leader relies on the counter to stop scaling. If the
  %% above part takes too long, it will overshoot by large margin.
  emqttb_metrics:counter_inc(?GROUP_N_WORKERS(my_group()), 1),
  try apply(Behavior, init, [my_settings()]) of
    State -> loop(State)
  catch
    EC:Err:Stack ->
      logger:error("[~p:~p] init ~p:~p:~p", [my_group(), my_id(), EC, Err, Stack]),
      terminate({Err, Stack})
  end.

loop(State) ->
  receive
    {'EXIT', _Pid, Reason} = Exit ->
      Reason =:= shutdown orelse
        logger:error("[~p:~p] received ~p", [my_group(), my_id(), Exit]),
      terminate(State, Reason);
    Msg ->
      try apply(behavior(), handle_message, [my_settings(), State, Msg]) of
        {ok, NewState} ->
          loop(NewState);
        {exit, NewState} ->
          terminate(NewState, normal);
        _ ->
          terminate(State, badreturn)
      catch
        EC:Err:Stack ->
          logger:error( "[~p:~p] handle_message ~p ~p:~p:~p"
                      ,  [my_group(), my_id(), Msg, EC, Err, Stack]
                      ),
          terminate(State, {Err, Stack})
      end
  end.

%%================================================================================
%% Internal functions
%%================================================================================

my_settings() ->
  persistent_term:get(?GROUP_BEHAVIOR_SHARED_STATE(group_leader())).

-spec behavior() -> module().
behavior() ->
  persistent_term:get(?GROUP_BEHAVIOR(group_leader())).

-spec terminate(_State, _Reason) -> no_return().
terminate(State, Reason) ->
  _ = catch apply(behavior(), terminate, [my_settings(), State]),
  terminate(Reason).

-spec terminate(_Reason) -> no_return().
terminate(Reason) ->
  emqttb_metrics:counter_dec(?GROUP_N_WORKERS(my_group()), 1),
  ?tp(emqttb_worker_terminate, #{gl => group_leader(), number => my_id()}),
  exit(Reason).

%% Connection

-spec connect_fun() -> FunName :: atom().
connect_fun()->
    case my_cfg([connection, transport]) of
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
       (Host)         -> {Host, my_cfg([broker, port])}
    end,
    my_cfg([broker, hosts])).
