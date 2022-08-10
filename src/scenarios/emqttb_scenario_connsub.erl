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
-module(emqttb_scenario_connsub).

-behavior(emqttb_scenario).

%% First all subscribers connect and subscribe to the brokers, then
%% the publishers start to connect and publish.  The default is to use
%% full forwarding of messages between the nodes: that is, each
%% publisher client publishes to a topic subscribed by a single
%% client, and both clients reside on distinct nodes.

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

-define(CONN_GROUP, pubsub_fwd_group_conn).
-define(SUB_GROUP, pubsub_fwd_group_sub).

%%================================================================================
%% behavior callbacks
%%================================================================================

name() ->
  connsub.

model() ->
  #{ sub =>
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
        #{ oneliner => "Starting worker number for this bench (used for multi-loadgen test alignment)"
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
  set_stage(connect),
  connect_stage(),
  set_stage(subscribe),
  subscribe_stage(),
  set_stage(loiter),
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

connect_stage() ->
  RandomHosts = my_conf([random_hosts]),
  HostSelection = case RandomHosts of
                      true -> random;
                      false -> round_robin
                  end,
  HostShift = case my_conf([full_forwarding]) of
                  true -> 1;
                  false -> 0
              end,
  ConnOpts = #{ metadata       => true
              , host_shift     => HostShift
              , host_selection => HostSelection
              },
  emqttb_group:ensure(#{ id            => ?CONN_GROUP
                       , client_config => my_conf([group])
                       , behavior      => {emqttb_behavior_conn, ConnOpts}
                       , start_n       => my_conf([start_n])
                       }),
  N = my_conf([num_clients]) div 2,
  Interval = my_conf([conninterval]),
  {ok, _} = emqttb_group:set_target(?CONN_GROUP, N, Interval),
  ok.

topic_prefix() ->
  <<"pubsub_connsub/">>.
