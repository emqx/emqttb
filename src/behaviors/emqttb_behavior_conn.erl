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
-module(emqttb_behavior_conn).

-behavior(emqttb_worker).

%% behavior callbacks:
-export([init_per_group/2, init/1, handle_message/3, terminate/2]).

-export_type([]).

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% behavior callbacks
%%================================================================================

init_per_group(_Group, Opts) ->
  CleanStart = maps:get(clean_start, Opts, true),
  HostShift = maps:get(host_shift, Opts, 0),
  HostSelection = maps:get(host_selection, Opts, random),
  #{ clean_start => CleanStart
   , host_shift => HostShift
   , host_selection => HostSelection
   }.

init(ConnOpts0 = #{clean_start := CleanStart}) ->
  ConnOpts = maps:with([host_shift, host_selection], ConnOpts0),
  {ok, Conn} = emqttb_worker:connect(ConnOpts, [{clean_start, CleanStart}], [], []),
  Conn.

handle_message(_ConnOpts, Conn, _Msg) ->
  {ok, Conn}.

terminate(_Shared, Conn) ->
  emqtt:disconnect(Conn),
  emqtt:stop(Conn).

%%================================================================================
%% Internal functions
%%================================================================================
