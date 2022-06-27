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
-module(emqttb_behavior_sub).

-behavior(emqttb_worker).

%% behavior callbacks:
-export([init_per_group/2, init/1, handle_message/3, terminate/2]).

-export_type([]).

%%================================================================================
%% Type declarations
%%================================================================================

-define(CNT_SUB_MESSAGES(GRP), {emqttb_received_messages, GRP}).
-define(AVG_SUB_TIME, subscribe).

%%================================================================================
%% behavior callbacks
%%================================================================================

init_per_group(Group,
               #{ topic  := Topic
                , qos    := QoS
                , expiry := Expiry
                }) when is_binary(Topic) ->
  SubCnt = emqttb_metrics:new_counter(?CNT_SUB_MESSAGES(Group),
                                      [ {help, <<"Number of received messages">>}
                                      , {labels, [group]}
                                      ]),
  emqttb_worker:new_opstat(Group, ?AVG_SUB_TIME),
  #{ topic       => Topic
   , sub_counter => SubCnt
   , qos         => QoS
   , expiry      => Expiry
   }.

init(#{topic := T, qos := QoS, expiry := Expiry}) ->
  Props = case Expiry of
            undefined -> #{};
            _         -> #{'Session-Expiry-Interval' => Expiry}
          end,
  {ok, Conn} = emqttb_worker:connect(Props),
  emqttb_worker:call_with_counter(?AVG_SUB_TIME, emqtt, subscribe, [Conn, emqttb_worker:format_topic(T), QoS]),
  Conn.

handle_message(#{sub_counter := Cnt}, Conn, {publish, #{client_pid := Pid}}) when
    Pid =:= Conn ->
  emqttb_metrics:counter_inc(Cnt, 1),
  {ok, Conn};
handle_message(_, Conn, _) ->
  {ok, Conn}.

terminate(_Shared, Conn) ->
  emqtt:disconnect(Conn),
  emqtt:stop(Conn).

%%================================================================================
%% Internal functions
%%================================================================================
