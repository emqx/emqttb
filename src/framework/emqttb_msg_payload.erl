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
-module(emqttb_msg_payload).

%% API:
-export([ on_receive_message/2
        , make_message/2
        , message_metadata/0
        , verify_sequence/4
        , ensure_sequence_table/0
        , metadata_size/1
        ]).

-export_type([sequence/0]).

%%================================================================================
%% Type declarations
%%================================================================================

-define(seq_tab, emqttb_behavior_sub_seq_tab).

-type sequence() :: {_From :: binary(), _To :: binary(), _Topic :: binary()}.

%%================================================================================
%% API functions
%%================================================================================

make_message(Msg, false) when is_binary(Msg) ->
  Msg;
make_message(Msg, true) when is_binary(Msg) ->
  [message_metadata(), Msg];
make_message(RandBytes, false) when is_integer(RandBytes) ->
  rand:bytes(RandBytes);
make_message(RandBytes, true) when is_integer(RandBytes) ->
  [message_metadata(), rand:bytes(RandBytes)].

on_receive_message(Conf = #{ parse_metadata := ParseMetadata
                           , verify_sequence := VerifySequence
                           , sub_counter := SubCnt
                           , e2e_latency := E2ELatency
                           },
                   #{payload := Payload, topic := Topic}) ->
  emqttb_metrics:counter_inc(SubCnt, 1),
  case ParseMetadata of
    true ->
      {Id, SeqNo, TS} = emqttb_behavior_pub:parse_metadata(Payload),
      Dt = os:system_time(microsecond) - TS,
      emqttb_metrics:rolling_average_observe(E2ELatency, Dt),
      case VerifySequence of
        true ->
          emqttb_msg_payload:verify_sequence(Conf, Id, Topic, SeqNo);
        false ->
          ok
      end;
    false ->
      ok
  end.

message_metadata() ->
  SeqNo = msg_seqno(),
  ID = erlang:phash2({node(), self()}),
  TS = os:system_time(microsecond),
  <<ID:32, SeqNo:64, TS:64>>.

verify_sequence(#{ number_of_gaps := NGaps, gap_size := GapSize, number_of_repeats := NRepeats
                 , repeat_size := RepeatSize, n_streams := NStreams},
                From, Topic, SeqNo) ->
  Key = {From, emqttb_worker:my_id(), Topic},
  case ets:lookup(?seq_tab, Key) of
    [] ->
      emqttb_metrics:counter_inc(NStreams, 1),
      ok;
    [{_, OldSeqNo}] when SeqNo =:= OldSeqNo + 1 ->
      ok;
    [{_, OldSeqNo}] when SeqNo > OldSeqNo ->
      logger:warning("Gap detected: ~p ~p; ~p", [OldSeqNo, SeqNo, Key]),
      emqttb_metrics:counter_inc(NGaps, 1),
      emqttb_metrics:rolling_average_observe(GapSize, SeqNo - OldSeqNo - 1);
    [{_, OldSeqNo}] ->
      logger:info("Repeat detected: ~p ~p; ~p", [OldSeqNo, SeqNo, Key]),
      emqttb_metrics:counter_inc(NRepeats, 1),
      emqttb_metrics:rolling_average_observe(RepeatSize, OldSeqNo - SeqNo + 1)
  end,
  ets:insert(?seq_tab, {Key, SeqNo}).

ensure_sequence_table() ->
  catch ets:new(?seq_tab,
                [ named_table
                , set
                , public
                , {write_concurrency, true}
                , {read_concurrency, true}
                , {heir, whereis(emqttb_metrics), ?seq_tab}
                ]),
  ok.

metadata_size(false) ->
  0;
metadata_size(true) ->
  (32 + 64 + 64) div 8.

%%================================================================================
%% behavior callbacks
%%================================================================================

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================

msg_seqno() ->
  case get(emqttb_behavior_pub_seqno) of
    undefined -> N = 0;
    N         -> ok
  end,
  put(emqttb_behavior_pub_seqno, N + 1),
  N.
