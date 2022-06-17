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
-module(emqttb).

%% API:
-export([main/1, terminate/0, setfail/1]).

%% behavior callbacks:
-export([]).

%% internal exports:
-export([n_clients/0]).

-export_type([n_clients/0]).

-include("emqttb.hrl").
-include_lib("typerefl/include/types.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type scenario() :: atom().

-type stage() :: atom().

-type group() :: atom().

-type n_clients() :: non_neg_integer().

-type autorate() :: atom().

-type interval() :: non_neg_integer() | {auto, autorate()}.

-type transport() :: sock | ws | quic.

-type proto_ver() :: v3 | v4 | v5.

-type qos() :: 0..2.

-reflect_type([scenario/0, stage/0, group/0, interval/0, transport/0, proto_ver/0, qos/0]).

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

n_clients() ->
  typerefl:range(0, erlang:system_info(process_limit) - 100).

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
