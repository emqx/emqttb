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
-export([my_group/0, my_id/0, my_clientid/0, my_hostname/0, my_cfg/1,
         connect/1, connect/4,
         format_topic/1,
         new_opstat/2, call_with_counter/4]).

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

-spec format_topic(binary()) -> binary().
format_topic(Pattern) ->
  Group = my_group(),
  ID = my_id(),
  Id0 = binary:replace(Pattern, <<"%n">>, integer_to_binary(ID), [global]),
  Id1 = binary:replace(Id0, <<"%g">>, atom_to_binary(Group), [global]),
  binary:replace(Id1, <<"%h">>, my_hostname(), [global]).

-spec connect(map()) -> gen_statem:start_ret().
connect(Properties) ->
  connect(Properties, [], [], []).

-spec connect(map(), [emqtt:option()], [gen_tcp:option()], [ssl:option()]) -> gen_statem:start_ret().
connect(Properties, CustomOptions, CustomTcpOptions, CustomSslOptions) ->
  Username = my_cfg([client, username]),
  Password = my_cfg([client, password]),
  SSL      = my_cfg([ssl, enable]),
  Options = [ {username,   Username} || Username =/= undefined]
         ++ [ {password,   Password} || Password =/= undefined]
         ++ [ {ssl_opts,   CustomSslOptions ++ ssl_opts()} || SSL]
         ++ [ {clientid,   my_clientid()}
            , {inflight,   my_cfg([connection, inflight])}
            , {hosts,      broker_hosts()}
            , {port,       get_port()}
            , {proto_ver,  my_cfg([connection, proto_ver])}
            , {low_mem,    my_cfg([lowmem])}
            , {owner,      self()}
            , {ssl,        SSL}
            , {tcp_opts,   CustomTcpOptions ++ tcp_opts()}
            , {properties, Properties}
            ],
  {ok, Client} = emqtt:start_link(CustomOptions ++ Options),
  ConnectFun = connect_fun(),
  {ok, _Properties} = call_with_counter(connect, emqtt, ConnectFun, [Client]),
  {ok, Client}.

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
  binary:replace(Id1, <<"%h">>, my_hostname(), [global]).

-spec my_hostname() -> binary().
my_hostname() ->
  atom_to_binary(node()).

-spec call_with_counter(atom(), module(), atom(), list()) -> _.
call_with_counter(Operation, Mod, Fun, Args) ->
  Grp = my_group(),
  emqttb_metrics:counter_inc(?GROUP_N_PENDING(Grp, Operation), 1),
  T0 = os:system_time(microsecond),
  try apply(Mod, Fun, Args)
  catch
    EC:Err ->
      EC(Err)
  after
    T = os:system_time(microsecond),
    emqttb_metrics:counter_dec(?GROUP_N_PENDING(Grp, Operation), 1),
    emqttb_metrics:gauge_observe(?GROUP_OP_TIME(Grp, Operation), T - T0)
  end.

-spec new_opstat(emqttb:group(), atom()) -> ok.
new_opstat(Group, Operation) ->
  emqttb_metrics:new_gauge(?GROUP_OP_TIME(Group, Operation),
                           [ {help, <<"Average run time of an operation (microseconds)">>}
                           , {labels, [group, operation]}
                           ]),
  emqttb_metrics:new_counter(?GROUP_N_PENDING(Group, Operation),
                             [ {help, <<"Number of pending operations">>}
                             , {labels, [group, operation]}
                             ]),
  ok.

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
      ?tp(error, emqttb_worker_crash,
          #{ callback  => init
           , group     => Group
           , worker_id => Number
           , error     => {EC, Err, Stack}
           }),
      terminate({Err, Stack})
  end.

loop(State) ->
  receive
    {'EXIT', _Pid, Reason} = Exit ->
      Reason =:= shutdown orelse
        ?tp(error, emqttb_worker_crash,
            #{ message   => Exit
             , group     => my_group()
             , worker_id => my_id()
             }),
      terminate(State, Reason);
    Msg ->
      try apply(behavior(), handle_message, [my_settings(), State, Msg]) of
        {ok, NewState} ->
          loop(NewState);
        {exit, NewState} ->
          terminate(NewState, normal);
        _Bad ->
          ?tp(error, emqttb_worker_crash,
              #{ callback  => handle_message
               , message   => Msg
               , state     => State
               , group     => my_group()
               , worker_id => my_id()
               , error     => {bad_return, _Bad}
               }),
          terminate(State, badreturn)
      catch
        EC:Err:Stack ->
          ?tp(error, emqttb_worker_crash,
              #{ callback  => handle_message
               , message   => Msg
               , state     => State
               , group     => my_group()
               , worker_id => my_id()
               , error     => {EC, Err, Stack}
               }),
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

get_port() ->
  case my_cfg([broker, port]) of
    Port when is_integer(Port) ->
      Port;
    default ->
      Transport = my_cfg([connection, transport]),
      SSL = my_cfg([ssl, enable]),
      case {Transport, SSL} of
        {sock, false} ->
          1883;
        {sock, true} ->
          8883;
        {ws, false} ->
          8083;
        {ws, true} ->
          8084;
        {quic, _} ->
          14567
      end
  end.

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
       (Host)         -> {Host, get_port()}
    end,
    my_cfg([broker, hosts])).

-spec tcp_opts() -> [gen_tcp:option()].
tcp_opts() ->
  [ {ifaddr, ifaddr()}
  ].

ifaddr() ->
  IfAddrs = my_cfg([net, ifaddr]),
  lists:nth(my_id() rem length(IfAddrs) + 1, IfAddrs).

-spec ssl_opts() -> [ssl:option()].
ssl_opts() ->
  Cert    = my_cfg([ssl, certfile]),
  Keyfile = my_cfg([ssl, keyfile]),
  [{certfile, Cert} || Cert =/= []] ++
  [{keyfile, Keyfile} || Keyfile =/= []] ++
  [{ciphers, all_ssl_ciphers()}].

all_ssl_ciphers() ->
  Vers = ['tlsv1', 'tlsv1.1', 'tlsv1.2', 'tlsv1.3'],
  lists:usort(lists:concat([ssl:cipher_suites(all, Ver) || Ver <- Vers])).
