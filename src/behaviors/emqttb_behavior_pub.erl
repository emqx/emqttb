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

%% behavior callbacks:
-export([init_per_group/2, init/1, handle_message/3, terminate/2]).

-export_type([]).

-import(emqttb_worker, [my_group/0, my_id/0, my_clientid/0, my_cfg/1, connect/2]).

-include("../framework/emqttb_internal.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-define(CNT_PUB_MESSAGES(GRP), {emqttb_published_messages, GRP}).
-define(AVG_PUB_TIME, publish).

%%================================================================================
%% behavior callbacks
%%================================================================================

init_per_group(Group,
               #{ topic       := Topic
                , pubinterval := PubInterval
                , msg_size    := MsgSize
                , qos         := QoS
                }) when is_binary(Topic),
                        is_integer(MsgSize) ->
  PubCnt = emqttb_metrics:new_counter(?CNT_PUB_MESSAGES(Group),
                                      [ {help, <<"Number of published messages">>}
                                      , {labels, [group]}
                                      ]),
  emqttb_worker:new_opstat(Group, ?AVG_PUB_TIME),
  {auto, PubRate} = emqttb_autorate:ensure(#{ id    => my_autorate(Group)
                                            , min   => PubInterval
                                            , error => fun() -> error_fun(Group) end
                                            , t_i   => 100
                                            , t_d   => 5
                                            , k_p   => 0.00001
                                            , max_control => 5
                                            }),
  #{ topic => Topic
   , message => message(MsgSize)
   , pub_counter => PubCnt
   , qos => QoS
   , pubinterval => PubRate
   }.

init(#{pubinterval := IRef}) ->
  {ok, Conn} = emqttb_worker:connect(#{}),
  I = counters:get(IRef, 1),
  rand:seed(default),
  set_timer(rand:uniform(I + 1)),
  Conn.

handle_message(Shared, Conn, publish) ->
  #{topic := T, pubinterval := IRef, message := Msg, pub_counter := Cnt, qos := QoS} = Shared,
  I = counters:get(IRef, 1),
  set_timer(I),
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

error_fun(Group) ->
  (emqttb_metrics:get_rolling_average(?GROUP_OP_TIME(Group, ?AVG_PUB_TIME)) -
     erlang:convert_time_unit(50, millisecond, microsecond)).
  %emqttb_metrics:get_counter(?GROUP_N_PENDING(Group, ?AVG_PUB_TIME)) - 30.

my_autorate(Group) ->
  list_to_atom("emqttb_pub_rate_" ++ atom_to_list(Group)).

set_timer(I) ->
  erlang:send_after(I, self(), publish).

message(Size) ->
  list_to_binary([$A || _ <- lists:seq(1, Size)]).
