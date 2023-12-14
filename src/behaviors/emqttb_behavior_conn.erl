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
-module(emqttb_behavior_conn).

-behavior(emqttb_worker).

%% API:
-export([model/1]).

%% behavior callbacks:
-export([init_per_group/2, init/1, handle_message/3, terminate/2]).

-export_type([prototype/0, config/0]).

%%================================================================================
%% Type declarations
%%================================================================================

-type config() :: #{ expiry => non_neg_integer()
                   , clean_start => boolean()
                   , metrics := lee:model_key()
                   }.

-type prototype() :: {?MODULE, config()}.

%%================================================================================
%% API
%%================================================================================

model(GroupId) ->
  #{ conn_latency =>
       emqttb_metrics:opstat(GroupId, connect)
   }.

%%================================================================================
%% behavior callbacks
%%================================================================================

init_per_group(_Group, Opts = #{metrics := MetricsKey}) ->
  Defaults = #{ expiry => 0
              , clean_start => true
              },
  Config = maps:merge(Defaults, Opts),
  Config#{ conn_opstat => emqttb_metrics:opstat_from_model(MetricsKey ++ [conn_latency])
         }.

init(#{clean_start := CleanStart, expiry := Expiry, conn_opstat := ConnOpstat}) ->
  Props = case Expiry of
            undefined -> #{};
            _         -> #{'Session-Expiry-Interval' => Expiry}
          end,
  {ok, Conn} = emqttb_worker:connect(ConnOpstat, Props, [{clean_start, CleanStart}], [], []),
  Conn.

handle_message(_, Conn, _) ->
  {ok, Conn}.

terminate(_Shared, Conn) ->
  emqtt:disconnect(Conn),
  emqtt:stop(Conn).

%%================================================================================
%% Internal functions
%%================================================================================
