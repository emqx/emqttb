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
-module(emqttb_scenario_pubsub_fwd).

-behavior(emqttb_scenario).

%% First all subscribers connect and subscribe to the brokers, then
%% the publishers start to connect and publish.  The default is to use
%% full forwarding of messages between the nodes: that is, each
%% publisher client publishes to a topic subscribed by a single
%% client, and both clients reside on distinct nodes.

%% behavior callbacks:
-export([ model/0
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

-define(PUB_GROUP, 'pubsub_fwd.pub').
-define(SUB_GROUP, 'pubsub_fwd.sub').

%%================================================================================
%% behavior callbacks
%%================================================================================

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
            {[value, cli_param, autorate],
             #{ oneliner => "Message publishing interval (microsecond)"
              , type => emqttb:duration_us()
              , default_ref => [interval]
              , cli_operand => "pubinterval"
              , cli_short => $i
              , autorate_id => 'pubsub_fwd/pubinterval'
              , process_variable => [?SK(pubsub_fwd), pub, pub_latency, pending]
              , error_coeff => -1
              }}
        , set_pub_latency =>
            {[value, cli_param],
             #{ oneliner => "Try to keep publishing time at this value (ms)"
              , type => emqttb:duration_ms()
              , default => 100
              , cli_operand => "publatency"
              }}
          %% Metrics:
        , n_published =>
            {[metric],
             #{ oneliner => "Total number of published messages"
              , id => {emqttb_published_messages, pubsub_fwd}
              , metric_type => counter
              , labels => [scenario]
              }}
        , pub_latency =>
            emqttb_metrics:opstat('pubsub_fwd/pub', 'publish')
        , conn_latency =>
            emqttb_metrics:opstat('pubsub_fdw/pub', 'connect')
        }
   , sub =>
       #{ qos =>
            {[value, cli_param],
             #{ oneliner => "Subscription QoS"
              , type => emqttb:qos()
              , default => 1
              , cli_operand => "sub-qos"
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
   , num_clients =>
       {[value, cli_param],
        #{ oneliner => "Total number of connections (pub + sub)"
         , type => emqttb:n_clients()
         , default => 100
         , cli_operand => "num-clients"
         , cli_short => $n
         }}
   , full_forwarding =>
       {[value, cli_param],
        #{ oneliner => "Whether all messages should be forwarded between nodes"
         , type => boolean()
         , default => true
         , cli_operand => "full-forwarding"
         }}
   , start_n =>
       {[value, cli_param],
        #{ oneliner => "Initial worker number for this bench (used for multi-loadgen test alignment)"
         , type => integer()
         , default => 0
         , cli_operand => "start-n"
         }}
   , random_hosts =>
       {[value, cli_param],
        #{ oneliner => "Whether to use random hosts rather than 1-shifted round-robin"
         , type => boolean()
         , default => false
         , cli_operand => "random-hosts"
         }}
   }.

run() ->
  set_stage(subscribe),
  subscribe_stage(),
  set_stage(publish),
  publish_stage(),
  loiter(),
  complete(ok).

%%================================================================================
%% Internal functions
%%================================================================================

subscribe_stage() ->
  TopicPrefix = topic_prefix(),
  RandomHosts = my_conf([random_hosts]),
  HostSelection = case RandomHosts of
                      true -> random;
                      false -> round_robin
                  end,
  SubOpts = #{ topic          => <<TopicPrefix/binary, "%n">>
             , qos            => my_conf([sub, qos])
             , expiry         => undefined
             , clean_start    => true
             , host_shift     => 0
             , host_selection => HostSelection
             , parse_metadata => true
             },
  emqttb_group:ensure(#{ id            => ?SUB_GROUP
                       , client_config => my_conf([group])
                       , behavior      => {emqttb_behavior_sub, SubOpts}
                       , start_n       => my_conf([start_n])
                       }),
  N = my_conf([num_clients]) div 2,
  Interval = my_conf([conninterval]),
  {ok, _} = emqttb_group:set_target(?SUB_GROUP, N, Interval),
  ok.

publish_stage() ->
  TopicPrefix = topic_prefix(),
  RandomHosts = my_conf([random_hosts]),
  HostSelection = case RandomHosts of
                      true -> random;
                      false -> round_robin
                  end,
  HostShift = case my_conf([full_forwarding]) of
                  true -> 1;
                  false -> 0
              end,
  PubOpts = #{ topic          => <<TopicPrefix/binary, "%n">>
             , n_published    => my_conf_key([pub, n_published])
             , pubinterval    => my_conf_key([pub, pubinterval])
             , msg_size       => my_conf([pub, msg_size])
             , qos            => my_conf([pub, qos])
             , set_latency    => my_conf_key([pub, set_pub_latency])
             , metadata       => true
             , host_shift     => HostShift
             , host_selection => HostSelection
             },
  emqttb_group:ensure(#{ id            => ?PUB_GROUP
                       , client_config => my_conf([group])
                       , behavior      => {emqttb_behavior_pub, PubOpts}
                       , start_n       => my_conf([start_n])
                       }),
  N = my_conf([num_clients]) div 2,
  Interval = my_conf([conninterval]),
  {ok, _} = emqttb_group:set_target(?PUB_GROUP, N, Interval),
  ok.

topic_prefix() ->
  <<"pubsub_fwd/">>.
