%%--------------------------------------------------------------------
%% Copyright (c) 2023 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(emqttb_logger).

%% API:
-export([setup/0]).

%% internal exports:
-export([filter_client/2, filter_default/2]).
-export([check_config/1, format/2]).

%%================================================================================
%% API funcions
%%================================================================================

setup() ->
  %% ok = logger:set_handler_config(default, formatter, {?MODULE, []}),
  ok = logger:add_handler(client, logger_std_h, #{config => #{file => "emqttb.log"}}),
  ok = logger:add_handler_filter(client, client_filter, {fun ?MODULE:filter_client/2, []}),
  ok = logger:add_handler_filter(default, client_filter, {fun ?MODULE:filter_default/2, []}).

%%================================================================================
%% Internal exports
%%================================================================================

filter_client(Event, _) ->
  case is_client_event(Event) of
    true ->
      Event;
    false ->
      stop
  end.

filter_default(Event, _) ->
  case is_client_event(Event) of
    true ->
      stop;
    false ->
      Event
  end.

%% Debug
check_config(_) ->
  ok.

%% Debug
format(LogEvent, _) ->
  io_lib:format("~p~n", [LogEvent]).

%%================================================================================
%% Internal functions
%%================================================================================

-compile({inline, [is_client_event/1]}).

is_client_event(#{meta := #{domain := [group|_]}}) ->
  true;
is_client_event(#{msg := {report, #{clientid := _}}}) ->
  true;
is_client_event(#{msg := {report, #{clietntid := _}}}) ->
  true;
is_client_event(#{msg := {report, #{modules := [emqtt]}}}) ->
  true;
is_client_event(#{msg := {report, #{report := [[{initial_call,{emqtt,_,_}}|_]|_]}}}) ->
  true;
is_client_event(_) ->
  false.
