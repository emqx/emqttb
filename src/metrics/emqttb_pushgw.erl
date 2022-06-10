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
-module(emqttb_pushgw).

%% API:
-export([start_link/0]).

%% behavior callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include("emqttb.hrl").

%%================================================================================
%% API funcions
%%================================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%================================================================================
%% behavior callbacks
%%================================================================================

init([]) ->
  start_timer(),
  {ok, []}.

handle_call(_Msg, _From, State) ->
  {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(collect, State) ->
  start_timer(),
  do_collect(),
  {noreply, State};
handle_info(_Msg, State) ->
  {noreply, State}.

%%================================================================================
%% Internal functions
%%================================================================================

start_timer() ->
  erlang:send_after(?CFG([metrics, pushgateway, interval]), self(), collect).

do_collect() ->
  Uri = ?CFG([metrics, pushgateway, url]),
  [Name, Ip] = string:tokens(atom_to_list(node()), "@"),
  Url = lists:concat([Uri, "/metrics/job/", Name, "/instance/", Name, "~", Ip]),
  Data = prometheus_text_format:format(),
  Headers = [],
  Options = [],
  {ok, Code, _RespHeaders, ClientRef} = hackney:post(Url, Headers, Data, Options),
  hackney:skip_body(ClientRef).
