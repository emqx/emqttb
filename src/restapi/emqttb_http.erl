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
-module(emqttb_http).

%% API:
-export([start_link/0, doc/0]).

-export_type([rest_options/0]).

-include("emqttb.hrl").

-import(lee_doc, [sect/3, p/1, li/2, href/2]).

%%================================================================================
%% Type declarations
%%================================================================================

-type rest_options() :: map().

%%================================================================================
%% API funcions
%%================================================================================

-include_lib("kernel/include/logger.hrl").

-spec start_link() -> {ok, pid()} | {error, _}.
start_link() ->
  {IP, Port} = ?CFG([restapi, listen_port]),
  logger:info("Starting REST API at ~p:~p", [IP, Port]),
  TransportOpts = #{ port => Port
                   , ip => IP
                   },
  Env = #{dispatch => dispatch()},
  ProtocolOpts = #{env => Env},
  StartFun = case ?CFG([restapi, tls]) of
               true ->
                 ?LOG_INFO("Starting HTTPS listener with parameters ~p", [ProtocolOpts]),
                 fun cowboy:start_tls/3;
               false ->
                 ?LOG_INFO("Starting HTTP listener with parameters ~p", [ProtocolOpts]),
                 fun cowboy:start_clear/3
             end,
  StartFun(rest_api, maps:to_list(TransportOpts), ProtocolOpts).

doc() ->
  [{listitem,
    [{para,
     [ href("http://localhost:" ++ integer_to_list(?DEFAULT_PORT) ++ Path, Path)
     , ": "
     , Mod:descr()
     ]}]}
   || {Path, Mod, _} <- routes()].

%%================================================================================
%% Internal functions
%%================================================================================

dispatch() ->
  cowboy_router:compile([{'_', routes()}]).

routes() ->
  RootDir = os:getenv("ROOTDIR"),
  [ {"/healthcheck",              emqttb_http_healthcheck, []}
  , {"/metrics",                  emqttb_http_metrics,     []}
  , {"/scenario/:scenario/stage", emqttb_http_stage,       []}
  , {"/conf/reload",              emqttb_http_sighup,      []}
  , {"/doc/[...]", cowboy_static, {dir, filename:join(RootDir, "doc/html")}}
  ].
