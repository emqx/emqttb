%%--------------------------------------------------------------------
%% Copyright (c) 2022-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
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

%% API:
-export([model/1]).

%% behavior callbacks:
-export([init_per_group/2, init/1, handle_message/3, terminate/2]).

-export_type([prototype/0, config/0]).

%%================================================================================
%% Type declarations
%%================================================================================

-type config() :: #{ topic           := binary()
                   , qos             := 0..2
                   , metrics         := lee:model_key()
                   , clean_start     => boolean()
                   , expiry          => non_neg_integer() | undefined
                   , host_shift      => integer()
                   , host_selection  => _
                   , parse_metadata  => boolean()
                   , verify_sequence => boolean()
                   }.

-type prototype() :: {?MODULE, config()}.

%%================================================================================
%% API
%%================================================================================

-spec model(atom()) -> lee:lee_module().
model(GroupId) ->
  #{ n_received =>
       {[metric],
        #{ oneliner => "Total number of received messages"
         , id => {emqttb_received_messages, GroupId}
         , metric_type => counter
         , labels => [group]
         }}
   , conn_latency =>
       emqttb_metrics:opstat(GroupId, 'connect')
   , sub_latency =>
       emqttb_metrics:opstat(GroupId, 'subscribe')

   , number_of_gaps =>
       {[metric],
        #{ oneliner => "Number of gaps in the sequence numbers"
         , metric_type => counter
         , id => {emqttb_gaps_number, GroupId}
         , labels => [group]
         }}
   , gap_size =>
       {[metric],
        #{ oneliner => "Average size of the gap in the sequence numbers"
         , metric_type => rolling_average
         , id => {emqttb_gap_size, GroupId}
         , labels => [group]
         }}

   , number_of_repeats =>
       {[metric],
        #{ oneliner => "Number of repeats of the sequence numbers"
         , metric_type => counter
         , id => {emqttb_repeats_number, GroupId}
         , labels => [group]
         }}
   , repeat_size =>
       {[metric],
        #{ oneliner => "Average size of the repeated sequence of seqence numbers"
         , metric_type => rolling_average
         , id => {emqttb_repeat_size, GroupId}
         , labels => [group]
         }}
   , n_streams =>
       {[metric],
        #{ oneliner => "Number of sequences"
         , metric_type => gauge
         , id => {emqttb_n_sequences, GroupId}
         , labels => [group]
         }}

   , e2e_latency =>
       {[metric],
        #{ oneliner => "End-to-end latency"
         , id => {emqttb_e2e_latency, GroupId}
         , metric_type => rolling_average
         , labels => [group]
         , unit => "microsecond"
         }}
   }.

%%================================================================================
%% behavior callbacks
%%================================================================================

init_per_group(_Group,
               #{ topic   := Topic0
                , qos     := _QoS
                , metrics := MetricsModelKey
                } = Opts) when is_binary(Topic0) ->
  Topic = emqttb_worker:split_topic(Topic0),
  ParseMetadata = maps:get(parse_metadata, Opts, false) orelse
                  maps:get(verify_sequence, Opts, false),
  Defaults = #{ expiry => 0
              , clean_start => true
              , host_shift => 0
              , host_selection => random
              , parse_metadata => ParseMetadata
              , verify_sequence => false
              },
  NStreams = emqttb_metrics:from_model(MetricsModelKey ++ [n_streams]),
  emqttb_metrics:gauge_set(NStreams, 0),
  Conf = maps:merge(Defaults, Opts),
  emqttb_msg_payload:ensure_sequence_table(),
  Conf#{ conn_opstat => emqttb_metrics:opstat_from_model(MetricsModelKey ++ [conn_latency])
       , sub_opstat => emqttb_metrics:opstat_from_model(MetricsModelKey ++ [sub_latency])
       , e2e_latency => emqttb_metrics:from_model(MetricsModelKey ++ [e2e_latency])
       , sub_counter => emqttb_metrics:from_model(MetricsModelKey ++ [n_received])
       , number_of_gaps => emqttb_metrics:from_model(MetricsModelKey ++ [number_of_gaps])
       , gap_size => emqttb_metrics:from_model(MetricsModelKey ++ [gap_size])
       , number_of_repeats => emqttb_metrics:from_model(MetricsModelKey ++ [number_of_repeats])
       , repeat_size => emqttb_metrics:from_model(MetricsModelKey ++ [repeat_size])
       , n_streams => NStreams
       , topic => Topic
       }.

init(SubOpts0 = #{ topic := T
                 , qos := QoS
                 , expiry := Expiry
                 , clean_start := CleanStart
                 , conn_opstat := ConnOpstat
                 , sub_opstat  := SubOpstat
                 }) ->
  SubOpts = maps:with([host_shift, host_selection], SubOpts0),
  Props = case Expiry of
            undefined -> SubOpts#{};
            _         -> SubOpts#{'Session-Expiry-Interval' => Expiry}
          end,
  {ok, Conn} = emqttb_worker:connect(ConnOpstat, Props, [{clean_start, CleanStart}], [], []),
  emqttb_metrics:call_with_counter(SubOpstat, emqtt, subscribe, [Conn, emqttb_worker:format_topic(T), QoS]),
  Conn.

handle_message(Conf, Conn, {publish, Msg = #{client_pid := Pid}}) when Pid =:= Conn ->
  emqttb_msg_payload:on_receive_message(Conf, Msg),
  {ok, Conn};
handle_message(_, Conn, _) ->
  {ok, Conn}.

terminate(_Shared, Conn) ->
  emqtt:disconnect(Conn),
  emqtt:stop(Conn).

%%================================================================================
%% Internal functions
%%================================================================================
