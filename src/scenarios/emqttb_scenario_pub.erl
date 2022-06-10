%%--------------------------------------------------------------------
%%Copyright (c) 2022 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-export([ name/0
        , model/0
        , run/0
        ]).

%% internal exports:
-export([]).

-export_type([]).

-include("emqttb.hrl").
-include_lib("typerefl/include/types.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% behavior callbacks
%%================================================================================

name() ->
  pub.

model() ->
  #{ topic =>
       {[value, cli_param],
        #{ oneliner => "Topic where the clients shall publish messages"
         , type => binary()
         , cli_operand => "topic"
         , cli_short => $t
         }}
   , connrate =>
       {[value, cli_param],
        #{ oneliner => "Client connection rate"
         , type => emqttb:rate()
         , default_ref => [rate]
         , cli_operand => "connrate"
         , cli_short => $r
         }}
   , pubrate =>
       {[value, cli_param],
        #{ oneliner => "Message publishing rate"
         , type => emqttb:rate()
         , default_ref => [rate]
         , cli_operand => "pubrate"
         , cli_short => $R
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
   }.

run() ->
  emqttb_group:ensure(#{ id => pub_group
                       , client_config => ?CFG([?SK, group])
                       , behavior => emqttb_behavior_pub
                       , max_size => ?CFG([?SK, n_clients])
                       }),
  ?STAGE(ramp_up),
  ok = emqttb_group:ramp_up(pub_group, ?CFG([?SK, n_clients]), 1),
  ?LINGER(),
  ?COMPLETE(ok).

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
