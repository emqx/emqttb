%%--------------------------------------------------------------------
%% Copyright (c) 2025 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(emqttb_behavior_pubsub).

-behavior(emqttb_worker).

%% API
-export([parse_metadata/1, model/1]).

%% behavior callbacks:
-export([init_per_group/2, init/1, handle_message/3, terminate/2]).

-export_type([prototype/0]).

-import(emqttb_worker, [send_after/2, send_after_rand/2, repeat/2,
                        my_group/0, my_id/0, my_clientid/0, my_cfg/1, connect/2]).

-include("../framework/emqttb_internal.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type prototype() :: {?MODULE, map()}.

%%================================================================================
%% API
%%================================================================================

-spec model(atom()) -> lee:lee_module().
model(Group) ->
  #{ conn_latency =>
       emqttb_metrics:opstat(Group, connect)
   , pub_latency =>
       emqttb_metrics:opstat(Group, publish)
   , sub_latency =>
       emqttb_metrics:opstat(Group, subscribe)
   , e2e_latency =>
       {[metric],
        #{ oneliner => "End-to-end latency"
         , id => {emqttb_e2e_latency, Group}
         , metric_type => rolling_average
         , labels => [group]
         , unit => "microsecond"
         }}
   , n_published =>
       {[metric],
        #{ oneliner => "Total number of messages published by the group"
         , id => {emqttb_published_messages, Group}
         , labels => [group]
         , metric_type => counter
         }}
   , n_received =>
       {[metric],
        #{ oneliner => "Total number of received messages"
         , id => {emqttb_received_messages, Group}
         , metric_type => counter
         , labels => [group]
         }}
   }.

-spec parse_metadata(Msg) -> {ID, SeqNo, TS}
          when Msg :: binary(),
               ID :: integer(),
               SeqNo :: non_neg_integer(),
               TS :: integer().
parse_metadata(<<ID:32, SeqNo:64, TS:64, _/binary>>) ->
  {ID, SeqNo, TS}.

%%================================================================================
%% behavior callbacks
%%================================================================================

init_per_group(_Group,
               #{ pub_topic      := PubTopic
                , sub_topic      := SubTopic
                , pubinterval    := PubInterval
                , msg_size       := MsgSize
                , pub_qos        := PubQoS
                , sub_qos        := SubQos
                , metrics        := MetricsKey
                , parse_metadata := ParseMetadata
                } = Conf) when is_binary(PubTopic),
                               is_binary(SubTopic),
                               is_integer(MsgSize) ->
  AddMetadata = maps:get(metadata, Conf, false),
  PubRate = emqttb_autorate:get_counter(emqttb_autorate:from_model(PubInterval)),
  MetadataSize = emqttb_msg_payload:metadata_size(AddMetadata),
  HostShift = maps:get(host_shift, Conf, 0),
  HostSelection = maps:get(host_selection, Conf, random),
  Size = max(0, MsgSize - MetadataSize),
  MsgPrototype = case maps:get(random, Conf, false) of
                   true ->
                     Size;
                   false ->
                     message(Size)
                 end,
  #{ pub_topic => PubTopic
   , sub_topic => SubTopic
   , sub_qos => SubQos
   , message_prototype => MsgPrototype
   , pub_opts => [{qos, PubQoS}, {retain, false}]
   , pubinterval => PubRate
   , metadata => AddMetadata
   , host_shift => HostShift
   , host_selection => HostSelection
   , expiry => maps:get(expiry, Conf, undefined)
   , clean_start => maps:get(clean_start, Conf, true)
   , pub_opstat => emqttb_metrics:opstat_from_model(MetricsKey ++ [pub_latency])
   , conn_opstat => emqttb_metrics:opstat_from_model(MetricsKey ++ [conn_latency])
   , sub_opstat => emqttb_metrics:opstat_from_model(MetricsKey ++ [sub_latency])
   , pub_counter => emqttb_metrics:from_model(MetricsKey ++ [n_published])
   , sub_counter => emqttb_metrics:from_model(MetricsKey ++ [n_received])
   , parse_metadata => ParseMetadata
   , verify_sequence => false
   , e2e_latency => emqttb_metrics:from_model(MetricsKey ++ [e2e_latency])
   }.

init(ClientOpts = #{pubinterval := I, conn_opstat := ConnOpstat,
                    sub_opstat := SubOpstat,
                    expiry := Expiry, clean_start := CleanStart,
                    sub_qos := SubQoS, sub_topic := SubTopic}) ->
  {SleepTime, N} = emqttb:get_duration_and_repeats(I),
  send_after_rand(SleepTime, {publish_, N}),
  HostShift = maps:get(host_shift, ClientOpts, 0),
  HostSelection = maps:get(host_selection, ClientOpts, random),
  Props = case Expiry of
            undefined -> #{};
            _         -> #{'Session-Expiry-Interval' => Expiry}
          end,
  {ok, Conn} = emqttb_worker:connect(ConnOpstat, Props#{host_shift => HostShift, host_selection => HostSelection},
                                     [{clean_start, CleanStart}], [], []),
  emqttb_metrics:call_with_counter(SubOpstat, emqtt, subscribe, [Conn, emqttb_worker:format_topic(SubTopic), SubQoS]),
  Conn.

handle_message(Shared, Conn, {publish_, N1}) ->
  #{ pub_topic := PubTopic
   , pubinterval := I
   , message_prototype := MsgProto
   , pub_opts := PubOpts
   , pub_counter := PubCounter
   , pub_opstat := PubOpstat
   , metadata := AddMetadata
   } = Shared,
  {SleepTime, N2} = emqttb:get_duration_and_repeats(I),
  send_after(SleepTime, {publish_, N2}),
  Msg = emqttb_msg_payload:make_message(MsgProto, AddMetadata),
  T = emqttb_worker:format_topic(PubTopic),
  repeat(N1, fun() ->
                 emqttb_metrics:call_with_counter(PubOpstat, emqtt, publish, [Conn, T, Msg, PubOpts]),
                 emqttb_metrics:counter_inc(PubCounter, 1)
             end),
  {ok, Conn};
handle_message(Conf, Conn, {publish, Msg = #{client_pid := Pid}}) when Pid =:= Conn ->
  emqttb_msg_payload:on_receive_message(Conf, Msg),
  {ok, Conn};
handle_message(_, Conn, _) ->
  {ok, Conn}.

terminate(_Shared, Conn) ->
  emqtt:disconnect(Conn).

%%================================================================================
%% Internal functions
%%================================================================================

message(Size) ->
  list_to_binary([$A || _ <- lists:seq(1, Size)]).
