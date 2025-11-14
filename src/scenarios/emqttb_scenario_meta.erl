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
-module(emqttb_scenario_meta).

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

-define(GROUP, meta).

%%================================================================================
%% behavior callbacks
%%================================================================================

model() ->
  #{ topic =>
       {[value, cli_param],
        #{ oneliner => "EMQTTB meta topic prefix"
         %% , doc => "@xref{Topic Patterns}\n"
         , type => binary()
         , default => <<"emqttb_meta">>
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
   , publish_interval =>
       {[value, cli_param],
        #{ oneliner => "Metrics publish interval"
         , type => emqttb:duration_ms()
         , default_str => "5s"
         , cli_operand => "interval"
         , cli_short => $i
         }}
   , conninterval =>
       {[value, cli_param, autorate],
        #{ oneliner => "Client connection interval (microsecond)"
         , type => emqttb:duration_us()
         , default => 1
         , cli_operand => "conninterval"
         , cli_short => $I
         , autorate_id => 'meta/conninterval'
         }}
   , metrics =>
       emqttb_behavior_sub:model('meta/meta')
   }.

initial_config() ->
  emqttb_conf:string2patch("@a -a meta/conninterval --pvar '[scenarios,meta,{},metrics,conn_latency,pending]' --olp").

run() ->
  Opts = #{ topic => my_conf([topic])
          , metrics => my_conf_key([metrics])
          , interval => my_conf([publish_interval])
          },
  emqttb_group:ensure(#{ id            => ?GROUP
                       , client_config => my_conf([group])
                       , behavior      => {emqttb_behavior_meta, Opts}
                       , conn_interval => emqttb_autorate:from_model(my_conf_key([conninterval]))
                       }),
  {ok, _} = emqttb_group:set_target(?GROUP, 1),
  set_stage(run_traffic),
  %% TODO: it should never stop
  loiter(),
  complete(ok).

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
