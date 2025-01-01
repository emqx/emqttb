%%--------------------------------------------------------------------
%% Copyright (c) 2022-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
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
         send_after/2, send_after_rand/2, repeat/2,
         connect/2, connect/5,
         format_topic/1
        ]).

%% internal exports:
-export([entrypoint/3, loop/1, start/3, init_per_group/3, model/0]).

-export_type([]).

-include("emqttb_internal.hrl").
-include_lib("snabbkaffe/include/trace.hrl").
-include_lib("typerefl/include/types.hrl").

-define(MY_ID, emqttb_my_id).
-define(MY_CLIENT, emqttb_my_client).

%%================================================================================
%% Behavior callbacks
%%================================================================================

%% Called once, during initializtion of the group. This callback
%% allows to create a static, shared behavior state.
-callback init_per_group(emqttb:group(), _BehaviorSettings) -> _Shared.

%% Called when the worker is started. For example, you can start an
%% MQTT client here. Avoid keeping static and shared terms in the
%% state, move it to `init_per_group' instead.
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

%%--------------------------------------------------------------------------------
%% Worker start/stop
%%--------------------------------------------------------------------------------

-spec start(module(), pid(), non_neg_integer()) -> pid().
start(Behavior, Group, Number) ->
  Options = [],
  spawn_opt(?MODULE, entrypoint, [Behavior, Group, Number], Options).

-spec init_per_group(module(), emqttb:group(), term()) -> term().
init_per_group(Module, GroupID, Opts) ->
  Module:init_per_group(GroupID, Opts).

%%--------------------------------------------------------------------------------
%% Getters/utilities
%%--------------------------------------------------------------------------------

-spec repeat(non_neg_integer(), fun(() -> _)) -> ok.
repeat(N, _) when N =< 0 ->
  ok;
repeat(N, Fun) ->
  Fun(),
  repeat(N - 1, Fun).

-spec send_after(non_neg_integer(), _Message) -> reference().
send_after(I, Message) ->
  erlang:send_after(I, self(), Message).

-spec send_after_rand(non_neg_integer(), _Message) -> reference().
send_after_rand(I, Message) when is_integer(I) ->
  erlang:send_after(rand:uniform(I + 1), self(), Message).

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

%% @doc Get group-specific configuration (as opposed to global)
-spec my_cfg(lee:key()) -> term().
my_cfg(Key) ->
  ConfKey = ?GROUP_CONF_ID(group_leader()),
  ConfId = persistent_term:get(ConfKey),
  ?CFG([groups, {ConfId} | Key]).

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
  {ok, Host} = inet:gethostname(),
  list_to_binary(Host).

%%--------------------------------------------------------------------------------
%% MQTT
%%--------------------------------------------------------------------------------

-spec connect(emqttb_metrics:metric_ref(), map()) -> gen_statem:start_ret().
connect(ConnOpstat, Properties) ->
  connect(ConnOpstat, Properties, [], [], []).

-spec connect(emqttb_metrics:metric_ref(), map(), [emqtt:option()], [gen_tcp:option()], [ssl:tls_option()]) -> gen_statem:start_ret().
connect(ConnOpstat, Properties0, CustomOptions, CustomTcpOptions, CustomSslOptions) ->
  HostShift = maps:get(host_shift, Properties0, 0),
  HostSelection = maps:get(host_selection, Properties0, random),
  Properties = maps:without([host_shift, host_selection], Properties0),
  Username    = my_cfg([client, username]),
  Password    = my_cfg([client, password]),
  SSL         = my_cfg([ssl, enable]),
  KeepAlive   = my_cfg([connection, keepalive]),
  MaxInflight = my_cfg([connection, inflight]),
  Options = [ {username,     Username} || Username =/= undefined]
         ++ [ {password,     Password} || Password =/= undefined]
         ++ [ {ssl_opts,     CustomSslOptions ++ ssl_opts()} || SSL]
         ++ [ {clientid,     my_clientid()}
            , {max_inflight, MaxInflight}
            , {hosts,        broker_hosts(HostSelection, HostShift)}
            , {port,         get_port()}
            , {proto_ver,    my_cfg([connection, proto_ver])}
            , {low_mem,      my_cfg([lowmem])}
            , {owner,        self()}
            , {ssl,          SSL}
            , {tcp_opts,     CustomTcpOptions ++ tcp_opts()}
            , {properties,   Properties #{'Receive-Maximum' => MaxInflight}}
            , {keepalive,    KeepAlive}
            ],
  {ok, Client} = emqtt:start_link(CustomOptions ++ Options),
  %% dbg:tracer(),
  %% dbg:p(Client, [c, s, r]),
  %% dbg:tpl({emqtt, '_', '_'}, x),
  %% logger:error("Debug ~p", [CustomOptions ++ Options]),
  ConnectFun = connect_fun(),
  {ok, _Properties} = emqttb_metrics:call_with_counter(ConnOpstat, emqtt, ConnectFun, [Client]),
  {ok, Client}.

%%================================================================================
%% Internal exports
%%================================================================================

-spec entrypoint(module(), emqttb:group(), integer()) -> no_return().
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
  logger:set_process_metadata(#{domain => [group, Group]}),
  try apply(Behavior, init, [my_settings()]) of
    State ->
          emqttb_group:report_live_id(my_group(), my_id()),
          loop(State)
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

model() ->
  #{ id =>
       {[value, cli_param],
        #{ oneliner    => "ID of the group"
         , type        => atom()
         , default     => default
         , cli_operand => "group"
         , cli_short   => $g
         }}
   , lowmem =>
       {[value, cli_param],
        #{ oneliner    => "Reduce memory useage at the cost of CPU wherever possible"
         , type        => boolean()
         , default     => false
         , cli_operand => "lowmem"
         }}
   , broker =>
       #{ hosts =>
            {[value, cli_param],
             #{ oneliner    => "Hostname of the target broker"
              , type        => emqttb:hosts()
              , default_str => "localhost"
              , cli_operand => "host"
              , cli_short   => $h
              }}
        , port =>
            {[value, cli_param],
             #{ oneliner    => "Port of the target broker"
              , type        => union(emqttb:net_port(), default)
              , default     => default
              , cli_operand => "port"
              , cli_short   => $p
              }}
        }
   , connection =>
       #{ proto_ver =>
            {[value, cli_param],
             #{ oneliner    => "MQTT protocol version"
              , type        => emqttb:proto_ver()
              , default     => v5
              , cli_operand => "version"
              , cli_short   => $V
              }}
        , transport =>
            {[value, cli_param],
             #{ oneliner    => "Transport protocol"
              , type        => emqttb:transport()
              , default     => sock
              , cli_operand => "transport"
              , cli_short   => $T
              }}
        , inflight =>
            {[value, cli_param],
             #{ oneliner    => "maximum inflight messages for QoS 1 and 2"
              , type        => union(non_neg_integer(), infinity)
              , default     => 10
              , cli_operand => "inflight"
              , cli_short   => $F
              }}
        , keepalive =>
            {[value, cli_param],
             #{ oneliner    => "Keepalive time"
              , doc         => "How often the clients will send @code{PING} MQTT message to the broker on idle connections."
              , type        => emqttb:duration_s()
              , default_str => "60s"
              , cli_operand => "keepalive"
              , cli_short   => $k
              }}
        }
   , client =>
       #{ clientid =>
            {[value, cli_param],
             #{ oneliner    => "ClientID pattern"
              , doc         => "@doc-clientid"
              , type        => binary()
              , default     => <<"%h-%g-%n">>
              , cli_operand => "clientid"
              , cli_short   => $i
              }}
        , username =>
            {[value, cli_param],
             #{ oneliner    => "Username of the client"
              , type        => union(undefined, string())
              , default     => undefined
              , cli_operand => "username"
              , cli_short   => $u
              }}
        , password =>
            {[value, cli_param],
             #{ oneliner    => "Password for connecting to the broker"
              , type        => union(undefined, string())
              , default     => undefined
              , cli_operand => "password"
              , cli_short   => $P
              }}
        }
   , net =>
       #{ ifaddr =>
            {[value, cli_param],
             #{ oneliner    => "Local IP addresses"
              , doc         => "@doc-ifaddr"
              , type        => emqttb:ifaddr_list()
              , default     => []
              , cli_operand => "ifaddr"
              }}
        }
   , ssl =>
       #{ enable =>
            {[value, cli_param],
             #{ oneliner    => "Enable SSL for the connections"
              , type        => boolean()
              , default     => false
              , cli_operand => "ssl"
              }}
        , certfile =>
            {[value, cli_param],
             #{ oneliner    => "Client certificate for authentication, if required by the server"
              , type        => string()
              , default     => ""
              , cli_operand => "certfile"
              }}
        , keyfile =>
            {[value, cli_param],
             #{ oneliner    => "Client private key for authentication, if required by the server"
              , type        => string()
              , default     => ""
              , cli_operand => "keyfile"
              }}
        , verify =>
            {[value, undocumented],
             #{ oneliner    => "If the client should validate the server identity"
              , type        => emqttb:ssl_verify()
              , default     => verify_none
              }}
        }
   }.

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
  emqttb_group:report_dead_id(my_group(), my_id()),
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

-spec broker_hosts(emqttb:host_selection(), integer()) -> [{string(), inet:port_number()}].
broker_hosts(random, _HostShift) ->
  Hosts = all_broker_hosts(),
  [X || {_Weight, X} <- lists:keysort(1, [{rand:uniform(), X} || X <- Hosts])];
broker_hosts(round_robin, HostShift) ->
  MyID = my_id(),
  Hosts = all_broker_hosts(),
  NHosts = length(Hosts),
  [lists:nth(1 + ((HostShift + MyID) rem NHosts), Hosts)].

-spec all_broker_hosts() -> [{string(), inet:port_number()}].
all_broker_hosts() ->
  lists:map(
    fun({Host, Port}) -> {Host, Port};
       (Host)         -> {Host, get_port()}
    end,
    my_cfg([broker, hosts])).

-spec tcp_opts() -> [gen_tcp:option()].
tcp_opts() ->
  SO_reuseaddr = ?CFG([inet, reuseaddr]),
  [ {reuseaddr, SO_reuseaddr}
  | ifaddr()
  ].

ifaddr() ->
  case my_cfg([net, ifaddr]) of
    [] ->
      [];
    IfAddrs ->
      IfAddr = lists:nth(my_id() rem length(IfAddrs) + 1, IfAddrs),
      [{ifaddr, IfAddr}]
  end.

-spec ssl_opts() -> [ssl:tls_option()].
ssl_opts() ->
  Cert    = my_cfg([ssl, certfile]),
  Keyfile = my_cfg([ssl, keyfile]),
  [{certfile, Cert} || Cert =/= []] ++
  [{keyfile, Keyfile} || Keyfile =/= []] ++
  [ {ciphers, all_ssl_ciphers()}
  , {verify, my_cfg([ssl, verify])}
  ].

all_ssl_ciphers() ->
  Vers = ['tlsv1', 'tlsv1.1', 'tlsv1.2', 'tlsv1.3'],
  lists:usort(lists:concat([ssl:cipher_suites(all, Ver) || Ver <- Vers])).
