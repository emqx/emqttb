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
-module(emqttb).

%% API:
-export([main/1, terminate/0, setfail/1, get_duration_and_repeats/1, duration_to_sleep/1]).

%% behavior callbacks:
-export([]).

%% internal exports:
-export([n_clients/0, parse_hosts/1, parse_addresses/1, parse_duration_us/1, parse_duration_ms/1,
         parse_byte_size/1, wait_time/0]).

-export_type([n_clients/0]).

-include("emqttb.hrl").
-include_lib("typerefl/include/types.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type duration_us() :: integer().
-typerefl_from_string({duration_us/0, ?MODULE, parse_duration_us}).

-type duration_ms() :: integer().
-typerefl_from_string({duration_ms/0, ?MODULE, parse_duration_ms}).

-type scenario() :: atom().

-type stage() :: atom().

-type group() :: atom().

-type n_clients() :: non_neg_integer().

-type autorate() :: atom().

-type transport() :: sock | ws | quic.

-type proto_ver() :: v3 | v4 | v5.

-type qos() :: 0..2.

-type ssl_verify() :: verify_peer | verify_none.

-type net_port() :: 1..65535.

-type byte_size() :: non_neg_integer().
-typerefl_from_string({byte_size/0, ?MODULE, parse_byte_size}).

-type hosts() :: [{string(), net_port()} | string()].
-typerefl_from_string({hosts/0, ?MODULE, parse_hosts}).

-type host_selection() :: round_robin | random.

-type ifaddr_list() :: nonempty_list(typerefl:ip_address()).
-typerefl_from_string({ifaddr_list/0, ?MODULE, parse_addresses}).

-reflect_type([scenario/0, stage/0, group/0, transport/0, proto_ver/0, qos/0,
               net_port/0, hosts/0, ifaddr_list/0, ssl_verify/0, host_selection/0,
               duration_ms/0, duration_us/0, byte_size/0]).

%%================================================================================
%% API funcions
%%================================================================================

%% Escript entrypoint
-spec main([string()]) -> no_return().
main(Args) ->
  application:set_env(emqttb, cli_args, Args),
  {ok, _} = application:ensure_all_started(?APP, permanent),
  %% Wait for completion of the scenarios:
  MRef = monitor(process, whereis(emqttb_scenarios_sup)),
  ?CFG([convenience, keep_running]) orelse
    emqttb_scenarios_sup:enable_autostop(),
  receive
    {'DOWN', MRef, _, _, Reason} ->
      Reason =/= shutdown andalso setfail(Reason),
      terminate()
  end.

setfail(Reason) ->
  application:set_env(emqttb, fail_reason, Reason),
  application:set_env(emqttb, is_fail, true).

%%================================================================================
%% Internal exports
%%================================================================================

wait_time() ->
  union(duration_ms(), infinity).

n_clients() ->
  typerefl:range(0, erlang:system_info(process_limit) - 100).

-spec duration_to_sleep(duration_us()) -> {non_neg_integer(), 1..1000}.
duration_to_sleep(0) ->
  {0, 1_000};
duration_to_sleep(DurationUs) when DurationUs >= 1_000 ->
  {DurationUs div 1_000, 1};
duration_to_sleep(DurationUs) ->
  {0, 1_000 div DurationUs}.

-spec get_duration_and_repeats(counters:counters_ref() | non_neg_integer()) ->
        {non_neg_integer(), 1..1000}.
get_duration_and_repeats(I) when is_integer(I) ->
  duration_to_sleep(I);
get_duration_and_repeats(CRef) ->
  get_duration_and_repeats(counters:get(CRef, 1)).

%%================================================================================
%% Internal functions
%%================================================================================

-spec terminate() -> no_return().
terminate() ->
  case application:get_env(emqttb, is_fail, false) of
    false ->
      timer:sleep(100), %% Ugly: give logger time to flush events...
      halt(0);
    true ->
      Reason = application:get_env(emqttb, fail_reason, ""),
      logger:critical("Run unsuccessful due to ~p", [Reason]),
      timer:sleep(100), %% Ugly: give logger time to flush events...
      halt(1)
  end.

parse_addresses(Str) ->
  L = [inet:parse_address(I) || I <- string:tokens(Str, ", ")],
  case lists:keyfind(error, 1, L) of
    false ->
      {ok, [I || {ok, I} <- L]};
    _ ->
      error
  end.

parse_duration_us(Str) ->
  {Int, Unit0} = string:to_integer(Str),
  Unit = string:trim(Unit0),
  case Unit of
    ""    -> {ok, Int * 1_000};
    "us"  -> {ok, Int};
    "μs"  -> {ok, Int};
    "ms"  -> {ok, Int * 1_000};
    "s"   -> {ok, Int * 1_000_000};
    "min" -> {ok, Int * 60_000_000};
    "h"   -> {ok, Int * 3600_000_000};
    _     -> error
  end.

parse_duration_ms(Str) ->
  {Int, Unit0} = string:to_integer(Str),
  Unit = string:trim(Unit0),
  case Unit of
    ""    -> {ok, Int};
    "ms"  -> {ok, Int};
    "s"   -> {ok, Int * 1_000};
    "min" -> {ok, Int * 60_000};
    "h"   -> {ok, Int * 3600_000};
    _     -> error
  end.

parse_byte_size(Str) ->
  {Int, Unit0} = string:to_integer(Str),
  Unit = string:trim(Unit0),
  case string:to_lower(Unit) of
    ""   -> {ok, Int};
    "kb" -> {ok, Int * 1024};
    "mb" -> {ok, Int * 1024 * 1024}
  end.

parse_hosts(Str) ->
  try
    {ok, [parse_host(I) || I <- string:tokens(Str, ", ")]}
  catch
    _:_ ->
      error
  end.

parse_host(Str) ->
  case string:tokens(Str, ":") of
    [Host] ->
      Host;
    [Host, Port] ->
      {Host, list_to_integer(Port)}
  end.
