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
-module(emqttb_http_healthcheck).

-export([ init/2
        , init/3
        , descr/0
        , handle_request/2
        , content_types_provided/2
        ]).

descr() ->
  "Healthcheck endpoint. Just returns 200 all the time.".

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
  {[{<<"text/plain">>, handle_request}], Req, State}.

handle_request(Req, State) ->
  {<<"">>, Req, State}.
