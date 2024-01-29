%%--------------------------------------------------------------------
%%Copyright (c) 2022-2024 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(emqttb_scenario_pub).

-behavior(emqttb_scenario).


%% behavior callbacks:
-export([ model/0
        , run/0
        , initial_config/0
        ]).

%% internal exports:
-export([]).

-export_type([]).

-include("emqttb.hrl").
-include_lib("typerefl/include/types.hrl").

-import(emqttb_scenario, [complete/1, loiter/0, my_conf_key/1, my_conf/1, set_stage/2, set_stage/1]).

%%================================================================================
%% Type declarations
%%================================================================================

-define(GROUP, 'pub').

%%================================================================================
%% behavior callbacks
%%================================================================================

model() ->
  #{ topic =>
       {[value, cli_param],
        #{ type => binary()
         , cli_operand => "topic"
         , cli_short => $t
         }}
   , qos =>
       {[value, cli_param],
        #{ oneliner => "QoS of the published messages"
         , type => emqttb:qos()
         , default => 0
         , cli_operand => "qos"
         , cli_short => $q
         }}
   , retain =>
       {[value, cli_param],
        #{ oneliner => "Retain published messages"
         , type => boolean()
         , default => false
         , cli_operand => "retain"
         }}
   , msg_size =>
       {[value, cli_param],
        #{ oneliner => "Size of the published message in bytes"
         , type => emqttb:byte_size()
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
   , conninterval =>
       {[value, cli_param, autorate],
        #{ oneliner => "Client connection interval (microsecond)"
         , type => emqttb:duration_us()
         , default_ref => [interval]
         , cli_operand => "conninterval"
         , cli_short => $I
         , autorate_id => 'pub/conninterval'
         }}
   , pubinterval =>
       {[value, cli_param, autorate],
        #{ oneliner => "Message publishing interval (microsecond)"
         , type => emqttb:duration_us()
         , default_ref => [interval]
         , cli_operand => "pubinterval"
         , cli_short => $i
         , autorate_id => 'pub/pubinterval'
         }}
   , n_clients =>
       {[value, cli_param],
        #{ oneliner => "Number of clients"
         , type => emqttb:n_clients()
         , default_ref => [n_clients]
         , cli_operand => "num-clients"
         , cli_short => $N
         }}
   , group =>
       {[value, cli_param, pointer],
        #{ oneliner => "ID of the client group"
         , type => atom()
         , default => default
         , cli_operand => "group"
         , cli_short => $g
         , target_node => [groups]
         }}
   , metadata =>
       {[value, cli_param],
        #{ oneliner    => "Add metadata to the messages"
         , type        => boolean()
         , default     => false
         , cli_operand => "metadata"
         }}
   , start_n =>
       {[value, cli_param],
        #{ oneliner => "Initial worker number for this bench (used for multi-loadgen test alignment)"
         , type => integer()
         , default => 0
         , cli_operand => "start-n"
         }}
   , expiry =>
       {[value, cli_param],
        #{ type => union(non_neg_integer(), undefined)
         , default => undefined
         , cli_operand => "expiry"
         , cli_short => $x
         }}
   , clean_start =>
       {[value, cli_param],
        #{ type => boolean()
         , default => true
         , cli_operand => "clean-start"
         , cli_short => $c
         }}
   , metrics =>
       emqttb_behavior_pub:model('pub/pub')
   }.

initial_config() ->
  emqttb_conf:string2patch("@a -a pub/pubinterval --pvar '[scenarios,pub,{},metrics,pub_latency,pending]'") ++
    emqttb_conf:string2patch("@a -a pub/conninterval --pvar '[scenarios,pub,{},metrics,conn_latency,pending]' --olp").

run() ->
  PubOpts = #{ topic       => my_conf([topic])
             , pubinterval => my_conf_key([pubinterval])
             , msg_size    => my_conf([msg_size])
             , qos         => my_conf([qos])
             , retain      => my_conf([retain])
             , metadata    => my_conf([metadata])
             , metrics     => my_conf_key([metrics])
             },
  emqttb_group:ensure(#{ id            => ?GROUP
                       , client_config => my_conf([group])
                       , behavior      => {emqttb_behavior_pub, PubOpts}
                       , start_n       => my_conf([start_n])
                       , conn_interval => emqttb_autorate:from_model(my_conf_key([conninterval]))
                       }),
  Interval = my_conf([conninterval]),
  set_stage(ramp_up),
  N = my_conf([n_clients]),
  {ok, _} = emqttb_group:set_target(?GROUP, N, Interval),
  set_stage(run_traffic),
  loiter(),
  complete(ok).

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
