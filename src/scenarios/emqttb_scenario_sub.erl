%%--------------------------------------------------------------------
%%Copyright (c) 2022-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(emqttb_scenario_sub).

-behavior(emqttb_scenario).

%% behavior callbacks:
-export([ model/0
        , initial_config/0
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

-define(GROUP, sub).

%%================================================================================
%% behavior callbacks
%%================================================================================

model() ->
  #{ topic =>
       {[value, cli_param],
        #{ oneliner => "Topic that the clients shall subscribe"
         , doc => "@xref{Topic Patterns}\n"
         , type => binary()
         , cli_operand => "topic"
         , cli_short => $t
         }}
   , conninterval =>
       {[value, cli_param, autorate],
        #{ oneliner => "Client connection interval"
         , type => emqttb:duration_us()
         , default_ref => [interval]
         , cli_operand => "conninterval"
         , cli_short => $I
         , autorate_id => 'sub/conninterval'
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
       {[value, cli_param],
        #{ oneliner => "ID of the client group"
         , type => atom()
         , default => default
         , cli_operand => "group"
         , cli_short => $g
         }}
   , expiry =>
       {[value, cli_param],
        #{ oneliner => "Set 'Session-Expiry' for persistent sessions (seconds)"
         , doc => "See @url{https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901048,Session Expiry Interval}.\n"
         , type => union(non_neg_integer(), undefined)
         , default => undefined
         , cli_operand => "expiry"
         , cli_short => $x
         }}
   , qos =>
       {[value, cli_param],
        #{ oneliner => "QoS of the subscription"
         , type => emqttb:qos()
         , default => 0
         , cli_operand => "qos"
         , cli_short => $q
         }}
   , parse_metadata =>
       {[value, cli_param],
        #{ oneliner => "Extract metadata from message payloads"
         , doc => "Subscribers will report end-to-end latency when this option is enabled.

                   @quotation Warning
                   Publishers should insert metadata into the payloads.
                   For example, when using @ref{value/scenarios/pub,pub scenario} it's necessary to enable @ref{value/scenarios/pub/_/metadata,metadata} generation.
                   @end quotation

                   @quotation Warning
                   In order to measure latency accurately, the scenario should ensure that publishers reside on the same emqttb host with the subscribers.
                   Otherwise clock skew between different load generator instances will introduce a systematic error.
                   @end quotation

                   "
         , type => boolean()
         , default => false
         , cli_operand => "parse-metadata"
         }}
   , verify_sequence =>
       {[value, cli_param],
        #{ oneliner => "Verify sequence of messages"
         , doc => "@xref{Verify Message Sequence}. Implies @ref{value/scenarios/sub/_/parse_metadata,parse_metadata}.\n"
         , type => boolean()
         , default => false
         , cli_operand => "verify-sequence"
         }}
   , clean_start =>
       {[value, cli_param],
        #{ oneliner => "Clean Start"
         , doc => "Note: in order to disable clean start (and make the session persistent) this flag should be set to @code{false}
                   (for example, @code{emqttb @@sub +c ...} via CLI).
                   "
         , type => boolean()
         , default => true
         , cli_operand => "clean-start"
         , cli_short => $c
         }}
   , metrics =>
       emqttb_behavior_sub:model('sub/sub')
   }.

initial_config() ->
  emqttb_conf:string2patch("@a -a sub/conninterval --pvar '[scenarios,sub,{},metrics,conn_latency,pending]' --olp").

run() ->
  SubOpts = #{ topic  => my_conf([topic])
             , qos    => my_conf([qos])
             , expiry => my_conf([expiry])
             , parse_metadata => my_conf([parse_metadata])
             , verify_sequence => my_conf([verify_sequence])
             , clean_start => my_conf([clean_start])
             , metrics => my_conf_key([metrics])
             },
  emqttb_group:ensure(#{ id            => ?GROUP
                       , client_config => my_conf([group])
                       , behavior      => {emqttb_behavior_sub, SubOpts}
                       , conn_interval => emqttb_autorate:from_model(my_conf_key([conninterval]))
                       }),
  set_stage(ramp_up),
  N = my_conf([n_clients]),
  {ok, _} = emqttb_group:set_target(?GROUP, N),
  set_stage(run_traffic),
  loiter(),
  complete(ok).

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
