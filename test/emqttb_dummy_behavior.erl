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
-module(emqttb_dummy_behavior).

-behavior(emqttb_worker).

%% behavior callbacks:
-export([create_settings/2, init/1, handle_message/3, terminate/2]).

-include_lib("snabbkaffe/include/trace.hrl").

-import(emqttb_worker, [my_group/0, my_id/0, my_clientid/0, my_cfg/1, connect/2]).

%%================================================================================
%% behavior callbacks
%%================================================================================

create_settings(Group, Cfg) ->
  Cfg#{group => Group}.

init(Shared) ->
  State = 0,
  ?tp(emqttb_dummy, #{ group    => my_group()
                     , id       => my_id()
                     , state    => State
                     , callback => ?FUNCTION_NAME
                     }),
  case maps:get(?FUNCTION_NAME, Shared, ok) of
    error ->
      error(deliberate);
    _ ->
      State
  end.

handle_message(Shared, State0, Msg) ->
  State = State0 + 1,
  ?tp(emqttb_dummy, #{ group    => my_group()
                     , id       => my_id()
                     , msg      => Msg
                     , state    => State
                     , callback => ?FUNCTION_NAME
                     }),
  case maps:get(?FUNCTION_NAME, Shared, ok) of
    ok ->
      {ok, State};
    error ->
      error(deliberate);
    invalid_return ->
      State
  end.

terminate(Shared, State0) ->
  State = State0 + 1,
  ?tp(emqttb_dummy, #{ group    => my_group()
                     , id       => my_id()
                     , state    => State
                     , callback => ?FUNCTION_NAME
                     }),
  case maps:get(?FUNCTION_NAME, Shared, ok) of
    error ->
      error(deliberate);
    _ ->
      State
  end.
