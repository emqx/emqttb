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
-module(emqttb_scenario_sub).

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
  sub.

model() ->
  #{ topic =>
       {[value, cli_param],
        #{ oneliner    => "Topic that the clients shall subscribe to"
         , type        => binary()
         , cli_operand => "topic"
         , cli_short   => $t
         }}
   , conninterval =>
       {[value, cli_param],
        #{ oneliner    => "Maximum client connection interval"
         , type        => emqttb:interval()
         , default_ref => [interval]
         , cli_operand => "connrate"
         , cli_short   => $r
         }}
   , n_subs =>
       {[value, cli_param],
        #{ oneliner    => "Number of clients"
         , type        => emqttb:n_clients()
         , default_ref => [n_clients]
         , cli_operand => "max-clients"
         , cli_short   => $N
         }}
   }.

run() ->
  ?STAGE(ramp_up),
  ?STAGE(run_traffic),
  ?LINGER(),
  ?COMPLETE(ok).

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
