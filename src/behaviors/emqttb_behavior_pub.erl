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
-module(emqttb_behavior_pub).

-behavior(emqttb_worker).

%% API
-export([parse_metadata/1]).

%% behavior callbacks:
-export([init_per_group/2, init/1, handle_message/3, terminate/2]).

-export_type([]).

-import(emqttb_worker, [send_after/2, send_after_rand/2,
                        my_group/0, my_id/0, my_clientid/0, my_cfg/1, connect/2]).

-include("../framework/emqttb_internal.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type config() :: #{ topic       := binary()
                   , pubinterval := counters:counters_ref()
                   , qos         := emqttb:qos()
                   , set_latency := lee:key()
                   , msg_size    := non_neg_integer()
                   , metadata    => boolean()
                   }.

%%================================================================================
%% API
%%================================================================================

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
               #{ topic       := Topic
                , pubinterval := PubInterval
                , msg_size    := MsgSize
                , qos         := QoS
                , set_latency := SetLatencyKey
                } = Conf) when is_binary(Topic),
                               is_integer(MsgSize),
                               is_list(SetLatencyKey) ->
  AddMetadata = maps:get(metadata, Conf, false),
  AutorateConf = maps:get(autorate, Conf, default),
  PubCnt = emqttb_metrics:new_counter(?CNT_PUB_MESSAGES(Group),
                                      [ {help, <<"Number of published messages">>}
                                      , {labels, [group]}
                                      ]),
  emqttb_worker:new_opstat(Group, ?AVG_PUB_TIME),
  {auto, PubRate} = emqttb_autorate:ensure(#{ id        => my_autorate(Group)
                                            , error     => fun() -> error_fun(SetLatencyKey, Group) end
                                            , init_val  => PubInterval
                                            , conf_root => AutorateConf
                                            }),
  MetadataSize = case AddMetadata of
                   true  -> 32 + 32 + 64;
                   false -> 0
                 end,
  HostShift = maps:get(host_shift, Conf, 0),
  #{ topic => Topic
   , message => message(MsgSize - MetadataSize)
   , pub_counter => PubCnt
   , qos => QoS
   , pubinterval => PubRate
   , metadata => AddMetadata
   , host_shift => HostShift
   }.

init(PubOpts = #{pubinterval := I}) ->
  rand:seed(default),
  send_after_rand(I, publish),
  HostShift = maps:get(host_shift, PubOpts, 0),
  {ok, Conn} = emqttb_worker:connect(#{host_shift => HostShift}),
  Conn.

handle_message(Shared, Conn, publish) ->
  #{ topic := TP, pubinterval := I, message := Msg0, pub_counter := Cnt
   , qos := QoS, metadata := AddMetadata
   } = Shared,
  send_after(I, publish),
  Msg = case AddMetadata of
          true  -> [message_metadata(), Msg0];
          false -> Msg0
        end,
  T = emqttb_worker:format_topic(TP),
  emqttb_worker:call_with_counter(?AVG_PUB_TIME, emqtt, publish, [Conn, T, Msg, QoS]),
  emqttb_metrics:counter_inc(Cnt, 1),
  {ok, Conn};
handle_message(_, Conn, _) ->
  {ok, Conn}.

terminate(_Shared, Conn) ->
  emqtt:disconnect(Conn).

%%================================================================================
%% Internal functions
%%================================================================================

error_fun(SetLatencyKey, Group) ->
  SetLatency = ?CFG(SetLatencyKey),
  AvgWindow = 250,
  %% Note that dependency of latency on publish interval is inverse:
  %% lesser interval -> messages are sent more often -> more load -> more latency
  %%
  %% So the control must be reversed, and error is the negative of what one usually
  %% expects:
  %% Current - Target instead of Target - Current.
  (emqttb_metrics:get_rolling_average(?GROUP_OP_TIME(Group, ?AVG_PUB_TIME), AvgWindow) -
     erlang:convert_time_unit(SetLatency, millisecond, microsecond)).

my_autorate(Group) ->
  list_to_atom("emqttb_pub_rate_" ++ atom_to_list(Group)).

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
