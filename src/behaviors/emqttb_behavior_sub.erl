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
-module(emqttb_behavior_sub).

-behavior(emqttb_worker).

%% behavior callbacks:
-export([init_per_group/2, init/1, handle_message/3, terminate/2]).

-export_type([prototype/0, config/0]).

%%================================================================================
%% Type declarations
%%================================================================================

-type config() :: #{ topic          := binary()
                   , qos            := 0..2
                   , clean_start    => boolean()
                   , expiry         => non_neg_integer() | undefined
                   , host_shift     => integer()
                   , host_selection => _
                   , parse_metadata => boolean()
                   }.

-type prototype() :: {?MODULE, config()}.

-define(CNT_SUB_MESSAGES(GRP), {emqttb_received_messages, GRP}).
-define(CNT_SUB_LATENCY(GRP), {emqttb_e2e_latency, GRP}).
-define(AVG_SUB_TIME, subscribe).

%%================================================================================
%% behavior callbacks
%%================================================================================

init_per_group(Group,
               #{ topic  := Topic
                , qos    := _QoS
                } = Opts) when is_binary(Topic) ->
  SubCnt = emqttb_metrics:new_counter(?CNT_SUB_MESSAGES(Group),
                                      [ {help, <<"Number of received messages">>}
                                      , {labels, [group]}
                                      ]),
  LatCnt = emqttb_metrics:new_rolling_average(?CNT_SUB_LATENCY(Group),
                                              [ {help, <<"End-to-end latency">>}
                                              , {labels, [group]}
                                              ]),
  emqttb_worker:new_opstat(Group, ?AVG_SUB_TIME),
  Defaults = #{ expiry => 0
              , clean_start => true
              , host_shift => 0
              , host_selection => random
              , parse_metadata => false
              },
  maps:merge(Defaults, Opts #{sub_counter => SubCnt, latency_counter => LatCnt}).

init(SubOpts0 = #{topic := T, qos := QoS, expiry := Expiry, clean_start := CleanStart}) ->
  SubOpts = maps:with([host_shift, host_selection], SubOpts0),
  Props = case Expiry of
            undefined -> SubOpts#{};
            _         -> SubOpts#{'Session-Expiry-Interval' => Expiry}
          end,
  {ok, Conn} = emqttb_worker:connect(Props, [{clean_start, CleanStart}], [], []),
  emqttb_worker:call_with_counter(?AVG_SUB_TIME, emqtt, subscribe, [Conn, emqttb_worker:format_topic(T), QoS]),
  Conn.

handle_message(#{sub_counter := Cnt, parse_metadata := ParseMetadata},
               Conn,
               {publish, #{client_pid := Pid, payload := Payload}}) when
    Pid =:= Conn ->
  emqttb_metrics:counter_inc(Cnt, 1),
  case ParseMetadata of
    true ->
      {_Id, _SeqNo, TS} = emqttb_behavior_pub:parse_metadata(Payload),
      Dt = os:system_time(microsecond) - TS,
      emqttb_metrics:rolling_average_observe(?CNT_SUB_LATENCY(emqttb_worker:my_group()), Dt);
    false ->
      ok
  end,
  {ok, Conn};
handle_message(_, Conn, _) ->
  {ok, Conn}.

terminate(_Shared, Conn) ->
  emqtt:disconnect(Conn),
  emqtt:stop(Conn).

%%================================================================================
%% Internal functions
%%================================================================================
