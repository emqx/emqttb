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


%% behavior callbacks:
-export([ name/0
        , model/0
        , run/0
        ]).

%% internal exports:
-export([]).

-export_type([]).

-include("emqttb.hrl").
-include_lib("typerefl/include/types.hrl").

-import(emqttb_scenario, [complete/1, loiter/0, my_conf/1, my_conf_key/1, set_stage/2, set_stage/1]).

%%================================================================================
%% Type declarations
%%================================================================================

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
             #{ oneliner => "Message publishing interval"
              , type => emqttb:interval()
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
        , set_latency =>
            {[value, cli_param],
             #{ oneliner => "Try to keep publishing time at this value (ms)"
              , type => integer()
              , default => 100
              , cli_operand => "publatency"
              }}
        }
   , sub =>
       #{ qos =>
            {[value, cli_param],
             #{ oneliner => "Subscription QoS"
              , type => emqttb:qos()
              , default => 1
              , cli_operand => "sub-qos"
              }}
        , n =>
            {[value, cli_param],
             #{ oneliner => "Number of subscribers"
              , type => emqttb:n_clients()
              , default_ref => [n_clients]
              , cli_operand => "num-subscribers"
              , cli_short => $S
              }}
        , disconnected_time =>
            {[value, cli_param],
             #{ oneliner => "The time clients spend while disconnected (ms)"
              , type => non_neg_integer()
              , default => 1_000
              , cli_operand => "t-disconnected"
              }}
        , realtime_latancy =>
            {[value, cli_param],
             #{ oneliner => "Maximum latency that we consider \"realtime\" (ms)"
              , type => non_neg_integer()
              , default => 100
              , cli_operand => "realtime"
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
        #{ oneliner => "Client connection interval"
         , type => emqttb:interval()
         , default_ref => [interval]
         , cli_operand => "conninterval"
         , cli_short => $I
         }}
   , topic_suffix =>
       {[value, cli_param],
        #{ oneliner => "Suffix of the topic to publish to"
         , type => binary()
         , default => <<"%h/%n">>
         , cli_operand => "topic"
         , cli_short => $t
         }}
   , group =>
       {[value, cli_param],
        #{ oneliner => "ID of the client group"
         , type => atom()
         , default => default
         , cli_operand => "group"
         , cli_short => $g
         }}
   }.

run() ->
  TopicPrefix = <<"pers_session/">>,
  TopicSuffix = my_conf([topic_suffix]),
  %% Create groups:
  PubOpts = #{ topic       => <<TopicPrefix/binary, TopicSuffix/binary>>
             , pubinterval => my_conf([pub, pubinterval])
             , msg_size    => my_conf([pub, msg_size])
             , qos         => my_conf([pub, qos])
             , set_latency => my_conf_key([pub, set_latency])
             , metadata    => true
             },
  emqttb_group:ensure(#{ id            => pers_sess_pub_group
                       , client_config => my_conf([group])
                       , behavior      => {emqttb_behavior_pub, PubOpts}
                       }),
  SubOpts = #{ topic  => <<TopicPrefix/binary, "#">>
             , qos    => my_conf([sub, qos])
             , disconnected_time => my_conf([sub, disconnected_time])
             , realitime_latency => my_conf([sub, realtime_latancy])
             , expiry => my_conf([sub, expiry])
             },
  emqttb_group:ensure(#{ id => pers_sess_sub_group
                       , client_config => my_conf([group])
                       , behavior      => {emqttb_behavior_sub_unsub, SubOpts}
                       }),
  %% Test:
  Interval = my_conf([conninterval]),
  set_stage(ramp_up),
  Npub = my_conf([pub, n]),
  Nsub = my_conf([sub, n]),
  emqttb_group:set_target_async(pers_sess_sub_group, Nsub, Interval),
  {ok, N} = emqttb_group:set_target(pers_sess_pub_group, Npub, Interval),
  set_stage(run_traffic),
  loiter(),
  complete(ok).

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
