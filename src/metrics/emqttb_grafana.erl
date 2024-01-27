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
-module(emqttb_grafana).

%% API:
-export([annotate/2, annotate/1, annotate_range/4]).

-export([parse_comma_separated/1]).

-include("emqttb.hrl").
-include_lib("typerefl/include/types.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type tags() :: [string()].
-typerefl_from_string({tags/0, ?MODULE, parse_comma_separated}).

-reflect_type([tags/0]).

%%================================================================================
%% API functions
%%================================================================================

-spec annotate(iolist()) -> ok.
annotate(Text) ->
  annotate(Text, []).

-spec annotate_range(iolist(), [string()], integer(), integer()) -> ok.
annotate_range(Text, Tags, Time, TimeEnd) ->
  Range = #{time => Time, 'timeEnd' => TimeEnd},
  annotate(Text, Tags, Range).

-spec annotate(iolist(), [atom() | string()]) -> ok.
annotate(Text, Tags0) ->
  annotate(Text, Tags0, #{}).

-spec annotate(iolist(), [atom() | string()], map()) -> ok.
annotate(Text, Tags0, Data) ->
  case ?CFG([metrics, grafana, enabled]) of
    true ->
      Tags = lists:map(fun ensure_string/1, Tags0),
      spawn(fun() -> do_annotate(Text, Tags, Data) end),
      ok;
    false ->
      ok
  end.

%%================================================================================
%% Internal functions
%%================================================================================

do_annotate(Text, Tags, Data0) ->
  Url = ?CFG([metrics, grafana, url]) ++ "/api/annotations",
  AuthToken = case ?CFG([metrics, grafana, api_key]) of
                false -> [];
                Token -> [{<<"Authorization">>, Token}]
              end,
  Headers = [{<<"Content-Type">>, <<"application/json">>} | AuthToken],
  Options = case AuthToken =:= [] andalso ?CFG([metrics, grafana, login]) of
              false -> [];
              Login -> [{basic_auth, {Login, ?CFG([metrics, grafana, password])}}]
            end,
  Data = Data0#{ text => iolist_to_binary(Text)
               , tags => [<<"emqttb">>, <<"mqtt">> | Tags]
               },
  {ok, Code, _RespHeaders, ClientRef} = hackney:post(Url, Headers, jsone:encode(Data), Options),
  case Code of
    200 ->
      hackney:skip_body(ClientRef);
    _ ->
      {ok, Body} = hackney:body(ClientRef),
      logger:warning("Grafana response code ~p: ~s", [Code, Body])
  end.

ensure_string(Atom) when is_atom(Atom) ->
  atom_to_binary(Atom);
ensure_string(Str) ->
  list_to_binary(Str).

parse_comma_separated(Str) ->
  {ok, string:tokens(Str, ",")}.
