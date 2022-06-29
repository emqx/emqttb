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
-module(emqttb_behavior_sub_unsub).

%% This behavior subscribes to a whildcard topic with persistent
%% session enabled, and then flips between the two states:
%%
%% - `disconnected'
%% - `connected'
%%
%% Transition between `disconnected' and `connected' states happens on
%% timer.
%%
%% Transition between `connected' and `disconnected' states happens
%% when the subscriber catches up with the realtime stream. That is,
%% when the average differnce of the timestamp baked into the received
%% messages becomes on average becomes close to zero.
%%
%% This convoluted way has been chosen, because it makes very little
%% assumptions about the order in which the messages are replayed by
%% the broker.

-behavior(emqttb_worker).

%% behavior callbacks:
-export([init_per_group/2, init/1, handle_message/3, terminate/2]).

-export_type([]).

-import(emqttb_worker, [send_after/2, send_after_rand/2,
                        my_group/0, my_id/0, my_clientid/0, my_cfg/1, connect/2]).

%%================================================================================
%% Type declarations
%%================================================================================

-define(CNT_SUB_MESSAGES(GRP), {emqttb_received_messages, GRP}).
-define(AVG_SUB_TIME, subscribe).

-type config() :: #{ topic := binary()
                   , qos := emqttb:qos()
                   , disconnected_time := counters:counters_ref() %% ms
                   , realtime_latancy := non_neg_integer() %% ms
                   , check_interval := non_neg_integer() %% ms
                   , expiry := non_neg_integer()
                   }.

-record(s,
        { state :: {connected, _Conn} | disconnected
        , avg_sum = 0 :: non_neg_integer()
        , avg_n = 0 :: non_neg_integer()
        }).

%%================================================================================
%% behavior callbacks
%%================================================================================

init_per_group(Group, Opts = #{ disconnected_time := DT
                              , topic := Topic
                              , qos := QoS
                              , realitime_latency := RealtimeLatency
                              , expiry := Expiry
                              }) ->
  SubCnt = emqttb_metrics:new_counter(?CNT_SUB_MESSAGES(Group),
                                      [ {help, <<"Number of received messages">>}
                                      , {labels, [group]}
                                      ]),
  true = DT < Expiry, % assert
  emqttb_worker:new_opstat(Group, ?AVG_SUB_TIME),
  #{ topic       => Topic
   , sub_counter => SubCnt
   , qos         => QoS
   , expiry      => Expiry
   , disconnected_time => DT
   , realitime_latency => RealtimeLatency
   }.

init(#{topic := Topic, qos := QoS, expiry := Expiry, disconnected_time := I}) ->
  Props = #{'Session-Expiry-Interval' => Expiry},
  {ok, Conn} = emqttb_worker:connect(Props),
  emqttb_worker:call_with_counter(?AVG_SUB_TIME, emqtt, subscribe, [Conn, {Topic, QoS}]),
  %% Dump messages from the previous session, if needed:
  #s{state = {connected, Conn}}.

handle_message(#{expiry := Expiry}, S, reconnect) ->
  Props = #{'Session-Expiry-Interval' => Expiry},
  {ok, Conn} = emqttb_worker:connect(Props),
  {ok, S#s{state = {connected, Conn}}};
handle_message(#{sub_counter := Cnt}, S = #s{state = {connected, Conn}},
               {publish, #{client_pid := Pid, payload := Payload}}) when
    Pid =:= Conn ->
  emqttb_metrics:counter_inc(Cnt, 1),
  {_ID, _SeqNo, TS} = emqttb_behavior_pub:parse_metadata(Payload),
  {ok, S}.

terminate(_Shared, Conn) ->
  emqtt:disconnect(Conn),
  emqtt:stop(Conn).

%%================================================================================
%% Internal functions
%%================================================================================
