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
-module(emqttb_scenario_sub_flapping).

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
         , autorate_id => 'sub_flapping/conninterval'
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
   , clean_start =>
       {[value, cli_param],
        #{ type => boolean()
         , default => true
         , cli_operand => "clean-start"
         , cli_short => $c
         }}
   , n_cycles =>
       {[value, cli_param],
        #{ type => emqttb:n_cycles()
         , default => 10
         , cli_operand => "cycles"
         , cli_short => $C
         }}
   , metrics =>
       emqttb_behavior_sub:model('sub_flapping/sub')
   }.

initial_config() ->
  emqttb_conf:string2patch("@a -a sub_flapping/conninterval --pvar '[scenarios,sub_flapping,{},metrics,conn_latency,pending]' --olp").

run() ->
  SubOpts = #{ topic  => my_conf([topic])
             , qos    => my_conf([qos])
             , expiry => my_conf([expiry])
             , clean_start => my_conf([clean_start])
             , metrics => my_conf_key([metrics])
             },
  emqttb_group:ensure(#{ id            => ?GROUP
                       , client_config => my_conf([group])
                       , behavior      => {emqttb_behavior_sub, SubOpts}
                       , conn_interval => emqttb_autorate:from_model(my_conf_key([conninterval]))
                       }),
  cycle(0, my_conf([n_cycles])).

cycle(Cycle, Max) when Cycle >= Max ->
  complete(ok);
cycle(Cycle, Max) ->
  set_stage(ramp_up),
  N = my_conf([n_clients]),
  {ok, _} = emqttb_group:set_target(?GROUP, N),
  set_stage(ramp_down),
  {ok, _} = emqttb_group:set_target(?GROUP, 0, undefined),
  cycle(Cycle + 1, Max).

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
