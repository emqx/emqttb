%%--------------------------------------------------------------------
%% Copyright (c) 2022-2024 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(emqttb_behavior_pub).

-behavior(emqttb_worker).

%% API
-export([parse_metadata/1, model/1]).

%% behavior callbacks:
-export([init_per_group/2, init/1, handle_message/3, terminate/2]).

-export_type([prototype/0, config/0]).

-import(emqttb_worker, [send_after/2, send_after_rand/2, repeat/2,
                        my_group/0, my_id/0, my_clientid/0, my_cfg/1, connect/2]).

-include("../framework/emqttb_internal.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type config() :: #{ topic          := binary()
                   , pubinterval    := lee:model_key()
                   , msg_size       := non_neg_integer()
                   , metrics        := lee:model_key()
                   , qos            := emqttb:qos()
                   , retain         => boolean()
                   , metadata       => boolean()
                   , host_shift     => integer()
                   , random         => boolean()
                   , host_selection => random | round_robin
                   , clean_start    => boolean()
                   , expiry         => non_neg_integer() | undefined
                   }.

-type prototype() :: {?MODULE, config()}.

%%================================================================================
%% API
%%================================================================================

-spec model(atom()) -> lee:namespace().
model(Group) ->
  #{ conn_latency =>
       emqttb_metrics:opstat(Group, connect)
   , pub_latency =>
       emqttb_metrics:opstat(Group, publish)
   , n_published =>
       {[metric],
        #{ oneliner => "Total number of messages published by the group"
         , id => {emqttb_published_messages, Group}
         , labels => [group]
         , metric_type => counter
         }}
   }.

-spec parse_metadata(Msg) -> {ID, SeqNo, TS}
          when Msg :: binary(),
               ID :: integer(),
               SeqNo :: non_neg_integer(),
               TS :: integer().
parse_metadata(<<ID:32, SeqNo:32, TS:64, _/binary>>) ->
  {ID, SeqNo, TS}.

%%================================================================================
%% behavior callbacks
%%================================================================================

init_per_group(Group,
               #{ topic        := Topic
                , pubinterval  := PubInterval
                , msg_size     := MsgSize
                , qos          := QoS
                , metrics      := MetricsKey
                } = Conf) when is_binary(Topic),
                               is_integer(MsgSize) ->
  AddMetadata = maps:get(metadata, Conf, false),
  PubRate = emqttb_autorate:get_counter(emqttb_autorate:from_model(PubInterval)),
  MetadataSize = case AddMetadata of
                   true  -> (32 + 32 + 64) div 8;
                   false -> 0
                 end,
  HostShift = maps:get(host_shift, Conf, 0),
  HostSelection = maps:get(host_selection, Conf, random),
  Retain = maps:get(retain, Conf, false),
  Size = max(0, MsgSize - MetadataSize),
  #{ topic => Topic
   , message => message(Size)
   , size => Size
   , pub_opts => [{qos, QoS}, {retain, Retain}]
   , pubinterval => PubRate
   , metadata => AddMetadata
   , host_shift => HostShift
   , host_selection => HostSelection
   , random => maps:get(random, Conf, false)
   , expiry => maps:get(expiry, Conf, undefined)
   , clean_start => maps:get(clean_start, Conf, true)
   , pub_opstat => emqttb_metrics:opstat_from_model(MetricsKey ++ [pub_latency])
   , conn_opstat => emqttb_metrics:opstat_from_model(MetricsKey ++ [conn_latency])
   , pub_counter => emqttb_metrics:from_model(MetricsKey ++ [n_published])
   }.

init(PubOpts = #{pubinterval := I, conn_opstat := ConnOpstat,
                 expiry := Expiry, clean_start := CleanStart}) ->
  rand:seed(default),
  {SleepTime, N} = emqttb:get_duration_and_repeats(I),
  send_after_rand(SleepTime, {publish, N}),
  HostShift = maps:get(host_shift, PubOpts, 0),
  HostSelection = maps:get(host_selection, PubOpts, random),
  Props = case Expiry of
            undefined -> #{};
            _         -> #{'Session-Expiry-Interval' => Expiry}
          end,
  {ok, Conn} = emqttb_worker:connect(ConnOpstat, Props#{host_shift => HostShift, host_selection => HostSelection},
                                    [{clean_start, CleanStart}], [], []),
  Conn.

handle_message(Shared, Conn, {publish, N1}) ->
  #{ topic := TP, pubinterval := I, message := Msg0, pub_opts := PubOpts
   , pub_counter := PubCounter
   , pub_opstat := PubOpstat
   , metadata := AddMetadata
   , random := Random
   , size := Size
   } = Shared,
  {SleepTime, N2} = emqttb:get_duration_and_repeats(I),
  send_after(SleepTime, {publish, N2}),
  Msg = if AddMetadata andalso Random -> [message_metadata(), rand:bytes(Size)];
           Random -> rand:bytes(Size);
           AddMetadata -> [message_metadata(), Msg0];
           true -> Msg0
        end,
  T = emqttb_worker:format_topic(TP),
  repeat(N1, fun() ->
                 emqttb_metrics:call_with_counter(PubOpstat, emqtt, publish, [Conn, T, Msg, PubOpts]),
                 emqttb_metrics:counter_inc(PubCounter, 1)
             end),
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

message_metadata() ->
  SeqNo = msg_seqno(),
  ID = erlang:phash2({node(), self()}),
  TS = os:system_time(microsecond),
  <<ID:32, SeqNo:32, TS:64>>.

msg_seqno() ->
  case get(emqttb_behavior_pub_seqno) of
    undefined -> N = 0;
    N         -> ok
  end,
  put(emqttb_behavior_pub_seqno, N + 1),
  N.
