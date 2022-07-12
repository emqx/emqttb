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
-module(emqttb_scenario_persistent_session).

-behavior(emqttb_scenario).

%% The scenario alternates between `subscribe_phase' and
%% `publish_phase'
%%
%% Publish stage ends after a set time interval
%%
%% Subscribe stage after the number of consumed messages per second
%% drops to 0
%%
%% KPIs:
%% - number of messages published during publish phase
%% - time to consume consume the messages
%% - subscription rate

%% behavior callbacks:
-export([ name/0
        , model/0
        , run/0
        ]).

%% internal exports:
-export([]).

-export_type([]).

-include("emqttb.hrl").
-include("../framework/emqttb_internal.hrl").
-include_lib("typerefl/include/types.hrl").

-import(emqttb_scenario, [complete/1, loiter/0, my_conf/1, my_conf_key/1, set_stage/2, set_stage/1]).

%%================================================================================
%% Type declarations
%%================================================================================

-define(PUB_GROUP, pers_sess_pub_group_pub).
-define(SUB_GROUP, pers_sess_pub_group_sub).

-define(PUB_THROUGHPUT, emqttb_pers_sess_pub_throughput).
-define(SUB_THROUGHPUT, emqttb_pers_sess_sub_throughput).

-record(s,
        { produced = 0
        , consumed = 0
        , to_consume = 0
        }).

%%================================================================================
%% behavior callbacks
%%================================================================================

name() ->
  persistent_session.

model() ->
  #{ pub =>
       #{ qos =>
            {[value, cli_param],
             #{ oneliner => "QoS of the published messages"
              , type => emqttb:qos()
              , default => 1
              , cli_operand => "pub-qos"
              }}
        , msg_size =>
            {[value, cli_param],
             #{ oneliner => "Size of the published message in bytes"
              , type => non_neg_integer()
              , cli_operand => "size"
              , cli_short => $s
              , default => 256
              }}
        , pubinterval =>
            {[value, cli_param],
             #{ oneliner => "Message publishing interval (microsecond)"
              , type => emqttb:duration_us()
              , default_ref => [interval]
              , cli_operand => "pubinterval"
              , cli_short => $i
              }}
        , n =>
            {[value, cli_param],
             #{ oneliner => "Number of publishers"
              , type => emqttb:n_clients()
              , default => 10
              , cli_operand => "num-publishers"
              , cli_short => $P
              }}
        , set_pub_latency =>
            {[value, cli_param],
             #{ oneliner => "Try to keep publishing time at this value (ms)"
              , type => emqttb:duration_ms()
              , default => 100
              , cli_operand => "publatency"
              }}
        , pub_autorate =>
            {[value, cli_param, pointer],
             #{ oneliner    => "ID of the autorate config used to tune publish interval"
              , type        => atom()
              , default     => default
              , cli_operand => "pubautorate"
              , target_node => [autorate]
              }}
        , topic_suffix =>
            {[value, cli_param],
             #{ oneliner => "Suffix of the topic to publish to"
              , type => binary()
              , default => <<"%h/%n">>
              , cli_operand => "topic"
              , cli_short => $t
              }}
        , pub_time =>
            {[value, cli_param],
             #{ oneliner => "Period of time while publishing will last (ms)"
              , type => non_neg_integer()
              , default => 5_0000
              , cli_operand => "pubtime"
              , cli_short => $T
              }}
        }
   , sub =>
       #{ qos =>
            {[value, cli_param],
             #{ oneliner => "Subscription QoS"
              , type => emqttb:qos()
              , default => 2
              , cli_operand => "sub-qos"
              }}
        , n =>
            {[value, cli_param],
             #{ oneliner => "Number of subscribers"
              , type => emqttb:n_clients()
              , default => 10
              , cli_operand => "num-subscribers"
              , cli_short => $S
              }}
        , expiry =>
            {[value, cli_param],
             #{ oneliner => "Session expiry interval"
              , type => non_neg_integer()
              , default => 16#FFFFFFFF
              , cli_operand => "expiry"
              }}
        }
   , conninterval =>
       {[value, cli_param],
        #{ oneliner => "Client connection interval (microsecond)"
         , type => emqttb:duration_us()
         , default => 0
         , cli_operand => "conninterval"
         , cli_short => $I
         }}
   , group =>
       {[value, cli_param],
        #{ oneliner => "ID of the client group"
         , type => atom()
         , default => default
         , cli_operand => "group"
         , cli_short => $g
         }}
   , n_cycles =>
       {[value, cli_param],
        #{ oneliner => "How many times to repeat publish/consume cycle"
         , type => union(non_neg_integer(), inifinity)
         , default => 10
         , cli_operand => "cycles"
         , cli_short => $C
         }}
   }.

run() ->
  prometheus_summary:declare([ {name, ?PUB_THROUGHPUT}
                             , {help, <<"Write throughput for the persistent session">>}
                             ]),
  prometheus_summary:declare([ {name, ?SUB_THROUGHPUT}
                             , {help, <<"Read throughput for the persistent session">>}
                             ]),
  NProd = try emqttb_metrics:get_counter(?CNT_PUB_MESSAGES(?PUB_GROUP))
          catch _:_ -> 0
          end,
  NCons = try emqttb_metrics:get_counter(?CNT_SUB_MESSAGES(?SUB_GROUP))
          catch _:_ -> 0
          end,
  S = #s{produced = NProd, consumed = NCons},
  do_run(S, 0).

%%================================================================================
%% Internal functions
%%================================================================================

do_run(S0, N) ->
  case N < my_conf([n_cycles]) of
    true ->
      set_stage(consume),
      S1 = consume_stage(N, S0),
      set_stage(publish),
      S = publish_stage(S1),
      do_run(S, N + 1);
    false ->
      complete(ok)
  end.

consume_stage(Cycle, S) ->
  TopicPrefix = topic_prefix(),
  SubOpts = #{ topic       => <<TopicPrefix/binary, "#">>
             , qos         => my_conf([sub, qos])
             , expiry      => my_conf([sub, expiry])
             , clean_start => Cycle =:= 0
             },
  emqttb_group:ensure(#{ id            => ?SUB_GROUP
                       , client_config => my_conf([group])
                       , behavior      => {emqttb_behavior_sub, SubOpts}
                       }),
  N = my_conf([sub, n]),
  Interval = my_conf([conninterval]),
  {ok, N} = emqttb_group:set_target(?SUB_GROUP, N, Interval),
  wait_consume_all(N, S),
  emqttb_group:stop(?SUB_GROUP),
  S#s{ to_consume = 0
     , consumed   = emqttb_metrics:get_counter(?CNT_SUB_MESSAGES(?SUB_GROUP))
     }.

publish_stage(S = #s{produced = NPub0}) ->
  TopicPrefix = topic_prefix(),
  TopicSuffix = my_conf([pub, topic_suffix]),
  PubOpts = #{ topic       => <<TopicPrefix/binary, TopicSuffix/binary>>
             , pubinterval => my_conf([pub, pubinterval])
             , msg_size    => my_conf([pub, msg_size])
             , qos         => my_conf([pub, qos])
             , set_latency => my_conf_key([pub, set_pub_latency])
             , metadata    => true
             },
  emqttb_group:ensure(#{ id            => ?PUB_GROUP
                       , client_config => my_conf([group])
                       , behavior      => {emqttb_behavior_pub, PubOpts}
                       }),
  N = my_conf([pub, n]),
  Interval = my_conf([conninterval]),
  {ok, N} = emqttb_group:set_target(?PUB_GROUP, N, Interval),
  PubTime = my_conf([pub, pub_time]),
  timer:sleep(PubTime),
  emqttb_group:stop(?PUB_GROUP),
  NPub = emqttb_metrics:get_counter(?CNT_PUB_MESSAGES(?PUB_GROUP)),
  %% TODO: it doesn't take ramp up/down into account:
  prometheus_summary:observe(?PUB_THROUGHPUT, (NPub - NPub0) / PubTime * timer:seconds(1)),
  S#s{produced = NPub, to_consume = NPub - NPub0}.

wait_consume_all(Nsubs, #s{to_consume = Nmsgs, consumed = Consumed}) ->
  do_consume(Consumed + Nsubs * Nmsgs).

do_consume(Target) ->
  timer:sleep(10),
  N = emqttb_metrics:get_counter(?CNT_SUB_MESSAGES(?SUB_GROUP)),
  if N >= Target ->
      N;
     true ->
      %logger:error("~p -> ~p", [N, Target]),
      do_consume(Target)
  end.

topic_prefix() ->
  <<"pers_session/">>.
