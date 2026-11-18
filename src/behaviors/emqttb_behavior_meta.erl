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
-module(emqttb_behavior_meta).

-behavior(emqttb_worker).

%% API:
-export([model/1]).

%% behavior callbacks:
-export([init_per_group/2, init/1, handle_message/3, terminate/2]).

-export_type([prototype/0, config/0]).

%%================================================================================
%% Type declarations
%%================================================================================

-type config() :: #{topic := binary(), interval := pos_integer(), metrics := _}.

-type prototype() :: {?MODULE, config()}.

%%================================================================================
%% API
%%================================================================================

-spec model(atom()) -> lee:lee_module().
model(_GroupId) ->
  #{
   }.

%%================================================================================
%% behavior callbacks
%%================================================================================

init_per_group(_Group, Opts = #{metrics := Metrics, topic := _, interval := _}) ->
  Defaults = #{ host_shift => 0
              , host_selection => random
              },
  Conf = maps:merge(Defaults, Opts),
  Conf#{conn_opstat => emqttb_metrics:opstat_from_model(Metrics ++ [conn_latency])}.

init(SubOpts0 = #{topic := T, interval := I, conn_opstat := ConnOpstat}) ->
  Props = maps:with([host_shift, host_selection], SubOpts0),
  {ok, Conn} = emqttb_worker:connect(ConnOpstat, Props, [{clean_start, true}], [], []),
  Topics = [{<<T/binary, "/#">>, [{nl, true}, {qos, 1}]}],
  emqtt:subscribe(Conn, #{}, Topics),
  schedule_pub(I),
  Conn.

handle_message(_Conf, Conn, {publish, Msg = #{client_pid := Pid}}) when Pid =:= Conn ->
  logger:notice("~p~n", [Msg]),
  {ok, Conn};
handle_message(#{interval := I, topic := T}, Conn, {timeout, _, push}) ->
  logger:debug("Push metadata~n", []),
  lists:foreach(
    fun(#{'$id' := Id, value := Value, error := Err}) ->
        Prefix = <<T/binary, "/autorate/", (atom_to_binary(Id))/binary, "/">>,
        emqtt:publish(Conn, <<Prefix/binary, "value">>, n2b(Value), 0),
        emqtt:publish(Conn, <<Prefix/binary, "error">>, n2b(Err), 0)
    end,
    emqttb_autorate:info()),
  schedule_pub(I),
  {ok, Conn};
handle_message(_, Conn, _) ->
  {ok, Conn}.

terminate(_Shared, Conn) ->
  emqtt:disconnect(Conn),
  emqtt:stop(Conn).

%%================================================================================
%% Internal functions
%%================================================================================

schedule_pub(SleepTime) ->
  erlang:start_timer(SleepTime, self(), push).

n2b(Int) when is_integer(Int) ->
  integer_to_binary(Int);
n2b(Float) when is_float(Float) ->
  list_to_binary(float_to_list(Float)).
