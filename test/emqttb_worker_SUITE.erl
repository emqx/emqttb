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
-module(emqttb_worker_SUITE).


-compile(nowarn_export_all).
-compile(export_all).

-include_lib("snabbkaffe/include/snabbkaffe.hrl").
-include_lib("stdlib/include/assert.hrl").

suite() ->
  [{timetrap, {seconds, 30}}].

all() ->
  snabbkaffe:mk_all(?MODULE).

init_per_testcase(_, Config) ->
  snabbkaffe:fix_ct_logging(),
  {ok, _} = application:ensure_all_started(emqttb),
  Config.

end_per_testcase(_, _Config) ->
  application:stop(emqttb),
  snabbkaffe:stop(),
  ok.

t_group(Config) ->
  NClients = 10,
  Group = test_group,
  ?check_trace(
     #{timeout => 1000},
     begin
       {ok, Pid} = emqttb_group:start_link(#{ id            => Group
                                            , client_config => #{}
                                            , behavior      => {emqttb_dummy_behavior, #{}}
                                            }),
       {ok, NActual} = emqttb_group:set_target(Group, NClients, 1),
       ?assert(NActual >= NClients),
       emqttb_group:broadcast(Group, message1),
       emqttb_group:broadcast(Group, message2),
       unlink(Pid),
       exit(Pid, shutdown)
     end,
     [ fun ?MODULE:worker_counter_spec/1
     , fun ?MODULE:state_continuity_spec/1
     , fun ?MODULE:cb_terminate_spec/1
     , {"Check that broadcast reaches all the clients",
        fun(Trace) ->
            [?assert(
                ?strict_causality( #{?snk_kind := emqttb_group_broadcast, message := _Msg}
                                 , #{?snk_kind := emqttb_dummy, id := Id, msg     := _Msg} when Id =:= N
                                 , Trace
                                 ))
             || N <- lists:seq(0, NClients - 1)],
            true
        end}
     ]).

t_error_in_init(Config) ->
  NClients = 10,
  Group = test_group,
  ?check_trace(
     #{timeout => 1000},
     begin
       {ok, Pid} = emqttb_group:start_link(#{ id            => Group
                                            , client_config => #{}
                                            , behavior      => {emqttb_dummy_behavior, #{init => error}}
                                            }),
       {ok, NActual} = emqttb_group:set_target(Group, NClients, 1),
       unlink(Pid),
       exit(Pid, shutdown)
     end,
     [ fun ?MODULE:worker_counter_spec/1
     ]).

t_error_in_handle_msg(Config) ->
  NClients = 10,
  Group = test_group,
  ?check_trace(
     #{timeout => 1000},
     begin
       {ok, Pid} = emqttb_group:start_link(#{ id            => Group
                                            , client_config => #{}
                                            , behavior      => {emqttb_dummy_behavior, #{handle_message => error}}
                                            }),
       {ok, NActual} = emqttb_group:set_target(Group, NClients, 1),
       unlink(Pid),
       exit(Pid, shutdown)
     end,
     [ fun ?MODULE:worker_counter_spec/1
     , fun ?MODULE:cb_terminate_spec/1
     ]).

t_invalid_return(Config) ->
  NClients = 10,
  Group = test_group,
  ?check_trace(
     #{timeout => 1000},
     begin
       {ok, Pid} = emqttb_group:start_link(#{ id            => Group
                                            , client_config => #{}
                                            , behavior      => {emqttb_dummy_behavior,
                                                                #{handle_message => invalid_return}}
                                            }),
       {ok, NActual} = emqttb_group:set_target(Group, NClients, 1),
       unlink(Pid),
       exit(Pid, shutdown)
     end,
     [ fun ?MODULE:worker_counter_spec/1
     , fun ?MODULE:cb_terminate_spec/1
     ]).

t_error_in_terminate(Config) ->
  NClients = 10,
  Group = test_group,
  ?check_trace(
     #{timeout => 1000},
     begin
       {ok, Pid} = emqttb_group:start_link(#{ id            => Group
                                            , client_config => #{}
                                            , behavior      => {emqttb_dummy_behavior,
                                                                #{terminate => error}}
                                            }),
       {ok, NActual} = emqttb_group:set_target(Group, NClients, 1),
       unlink(Pid),
       exit(Pid, shutdown)
     end,
     [ fun ?MODULE:worker_counter_spec/1
     , fun ?MODULE:cb_terminate_spec/1
     ]).

%% Trace specifications:

%% Ensure that worker counter is always decremented
worker_counter_spec(Trace) ->
  ?assert(
     ?strict_causality( #{?snk_kind := emqttb_worker_start,     gl := _GL, number := _N}
                      , #{?snk_kind := emqttb_worker_terminate, gl := _GL, number := _N}
                      , Trace
                      )).

%% Ensure that the worker state is passed through callbacks
state_continuity_spec(Trace) ->
  ?assert(
     ?causality( #{?snk_kind := emqttb_dummy, group := _G, id := _N, state := _S1}
               , #{?snk_kind := emqttb_dummy, group := _G, id := _N, state := _S2}
               , _S1 =:= _S2 + 1
               , Trace
               )).

%% Ensure that for every `init' there is a call of `terminate' callback
cb_terminate_spec(Trace) ->
  ?assert(
     ?strict_causality( #{?snk_kind := emqttb_dummy, group := _G, id := _N, callback := init}
                      , #{?snk_kind := emqttb_dummy, group := _G, id := _N, callback := terminate}
                      , Trace
                      )).
