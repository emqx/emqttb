%%--------------------------------------------------------------------
%% Copyright (c) 2025 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(emqttb_scenario_pubsub).

-behavior(emqttb_scenario).

%% First all subscribers connect and subscribe to the brokers, then
%% the publishers start to connect and publish.  The default is to use
%% full forwarding of messages between the nodes: that is, each
%% publisher client publishes to a topic subscribed by a single
%% client, and both clients reside on distinct nodes.

%% behavior callbacks:
-export([ model/0
        , run/0
        , initial_config/0
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

-define(GROUP, 'pubsub').

%%================================================================================
%% behavior callbacks
%%================================================================================

model() ->
  #{ group =>
       {[value, cli_param, pointer],
        #{ oneliner => "ID of the client group"
         , type => atom()
         , default => default
         , cli_operand => "group"
         , cli_short => $g
         , target_node => [groups]
         }}
   , pub =>
       #{ topic =>
            {[value, cli_param],
             #{ oneliner => "Topic used for publishing"
              , type => binary()
              , default => <<"pubsub/pub/%n">>
              , cli_operand => "pub-topic"
              }}
        , metadata =>
            {[value, cli_param],
             #{ oneliner    => "Add metadata to the messages"
              , type        => boolean()
              , default_ref => [scenarios, pubsub, {}, sub, parse_metadata]
              , cli_operand => "metadata"
              }}
        , qos =>
            {[value, cli_param],
             #{ oneliner    => "QoS of the published messages"
              , type        => emqttb:qos()
              , default     => 1
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
        , random =>
            {[value, cli_param],
             #{ oneliner => "Randomize message contents"
              , type => boolean()
              , cli_operand => "random"
              , default => false
              }}
        , pubinterval =>
            {[value, cli_param, autorate],
             #{ oneliner => "Message publishing interval (microsecond)"
              , type => emqttb:duration_us()
              , default_ref => [interval]
              , cli_operand => "pubinterval"
              , cli_short => $i
              , autorate_id => 'pubsub/pubinterval'
              }}
        }
   , sub =>
       #{ topic =>
            {[value, cli_param],
             #{ oneliner => "Topic that clients subscribe to"
              , type => binary()
              , default => <<"pubsub/sub/%n">>
              , cli_operand => "sub-topic"
              }}
        , qos =>
            {[value, cli_param],
             #{ oneliner => "QoS of the subscription"
              , type => emqttb:qos()
              , default => 1
              , cli_operand => "sub-qos"
              }}
        , parse_metadata =>
            {[value, cli_param],
             #{ oneliner => "Extract metadata from message payloads"
              , doc => "@xref{value/scenarios/sub/_/parse_metadata,parse_metadata}."
              , type => boolean()
              , default => false
              , cli_operand => "parse-metadata"
              }}
        }
   , metrics =>
       emqttb_behavior_pubsub:model('pubsub')
   , conninterval =>
       {[value, cli_param, autorate],
        #{ oneliner => "Client connection interval"
         , type => emqttb:duration_us()
         , cli_operand => "conninterval"
         , cli_short => $I
         , default_str => "10ms"
         , autorate_id => 'pubsub/conninterval'
         }}
   , num_clients =>
       {[value, cli_param],
        #{ oneliner => "Total number of connections"
         , type => emqttb:n_clients()
         , default => 100
         , cli_operand => "num-clients"
         , cli_short => $n
         }}
   , start_n =>
       {[value, cli_param],
        #{ oneliner => "Id of the first client in the group"
         , type => non_neg_integer()
         , default => 0
         , cli_operand => "start-n"
         }}
   , clean_start =>
       {[value, cli_param],
        #{ oneliner => "Set MQTT clean start flag"
         , doc => "@xref{value/scenarios/sub/_/clean_start}."
         , type => boolean()
         , default => true
         , cli_operand => "clean-start"
         }}
   , expiry =>
       {[value, cli_param],
        #{ oneliner => "Set 'Session-Expiry' for persistent sessions (seconds)"
         , doc => "@xref{value/scenarios/sub/_/clean_start}."
         , type => union(non_neg_integer(), undefined)
         , default => undefined
         , cli_operand => "expiry"
         , cli_short => $x
         }}
   }.

initial_config() ->
  emqttb_conf:string2patch("@a -a pubsub/pubinterval --pvar '[scenarios,pubsub,{},metrics,pub_latency,pending]'") ++
  emqttb_conf:string2patch("@a -a pubsub/conninterval --pvar '[scenarios,pubsub,{},metrics,conn_latency,pending]' --olp").

run() ->
  set_stage(ramp_up),
  ramp_up(),
  set_stage(loiter),
  loiter(),
  complete(ok).

%%================================================================================
%% Internal functions
%%================================================================================

ramp_up() ->
  Opts = #{ pub_topic      => my_conf([pub, topic])
          , sub_topic      => my_conf([sub, topic])
          , pubinterval    => my_conf_key([pub, pubinterval])
          , msg_size       => my_conf([pub, msg_size])
          , pub_qos        => my_conf([pub, qos])
          , sub_qos        => my_conf([sub, qos])
          , metrics        => my_conf_key([metrics])
          , metadata       => my_conf([pub, metadata])
          , random         => my_conf([pub, random])
          , expiry         => undefined
          , clean_start    => my_conf([clean_start])
          , host_shift     => 0
          , parse_metadata => my_conf([sub, parse_metadata])
          },
  emqttb_group:ensure(#{ id            => ?GROUP
                       , client_config => my_conf([group])
                       , behavior      => {emqttb_behavior_pubsub, Opts}
                       , start_n       => my_conf([start_n])
                       , conn_interval => emqttb_autorate:from_model(my_conf_key([conninterval]))
                       }),
  N = my_conf([num_clients]),
  {ok, _} = emqttb_group:set_target(?GROUP, N),
  ok.
