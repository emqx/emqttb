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
-module(emqttb_conf).

%% API:
-export([load_model/0, load_conf/0, get/1, list_keys/1, reload/0, patch/1, string2patch/1]).
-export([compile_model/1]).

-export_type([]).

-include("emqttb.hrl").
-include_lib("typerefl/include/types.hrl").


%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API funcions
%%================================================================================

load_model() ->
  case compile_model(emqttb_conf_model:model()) of
    {ok, Model} ->
      persistent_term:put(?MODEL_STORE, Model),
      ok;
    {error, Errors} ->
      logger:critical("Configuration model is invalid!"),
      [logger:critical(E) || E <- Errors],
      emqttb:setfail("invalid configuration model"),
      emqttb:terminate()
  end.

load_conf() ->
  Storage = lee_storage:new(lee_persistent_term_storage, ?CONF_STORE),
  case lee:init_config(?MYMODEL, Storage) of
    {ok, _Data, _Warnings} ->
      maybe_load_repeat(),
      maybe_load_conf_file(),
      maybe_dump_conf(),
      ok;
    {error, Errors, Warnings} ->
      [logger:critical(E) || E <- Errors],
      [logger:warning(E) || E <- Warnings],
      emqttb:setfail("invalid configuration"),
      emqttb:terminate()
  end.

compile_model(Model) ->
  MTs = metamodel(),
  lee_model:compile(MTs, [Model]).

reload() ->
  logger:notice("Reloading configuration"),
  case lee:init_config(?MYMODEL, ?MYCONF) of
    {ok, _, _} -> true;
    _          -> false
  end.

patch(Patch) ->
  logger:debug("Patching configuration: ~p", [Patch]),
  case lee:patch(?MYMODEL, ?MYCONF, Patch) of
    {ok, _, _} -> true;
    _          -> false
  end.

-spec get(lee:key()) -> term().
get(Key) ->
  ?CFG(Key).

-spec list_keys(lee:model_key()) -> [lee:key()].
list_keys(Key) ->
  ?CFG_LIST(Key).

%%================================================================================
%% Internal functions
%%================================================================================

maybe_dump_conf() ->
  case {?CFG([convenience, conf_dump]), ?CFG([convenience, again])} of
    {Filename, false} when is_list(Filename) ->
      Dump = lee_storage:dump(?MYCONF),
      {ok, FD} = file:open(Filename, [write]),
      [ok = io:format(FD, "~p.~n", [I]) || I <- Dump],
      ok = file:close(FD);
    _ ->
      ok
  end.

maybe_load_repeat() ->
  case {?CFG([convenience, conf_dump]), ?CFG([convenience, again])} of
    {Filename, true} when is_list(Filename) ->
      case file:consult(Filename) of
        {ok, Patch} ->
          lee:patch(?MYMODEL, ?MYCONF, Patch);
        _ ->
          ok
      end;
    _ ->
      ok
  end.

maybe_load_conf_file() ->
  case ?CFG([convenience, conf_file]) of
    undefined ->
      ok;
    File ->
      {ok, _, _} = lee_config_file:read_to(?MYMODEL, ?MYCONF, File)
  end.


metamodel() ->
  [ lee:base_metamodel()
  , lee_metatype:create(lee_os_env, #{prefix => "EMQTTB_", priority => 0})
  , lee_metatype:create(lee_app_env)
  , lee_metatype:create(lee_logger)
  , lee_metatype:create(lee_cli,
                        #{ cli_opts => fun cli_args_getter/0
                         , priority => 10
                         })
  , lee_metatype:create(lee_config_file,
                        #{ tag => system_wide_conf
                         , priority => -110
                         , file => "/etc/emqttb/emqttb.conf"
                         })
  , lee_metatype:create(emqttb_scenario)
  , lee_metatype:create(emqttb_metrics)
  , lee_metatype:create(emqttb_autorate)
  ].

cli_args_getter() ->
  application:get_env(emqttb, cli_args, []).

string2patch(Str) ->
  %% Lazy attempt to parse string to a list of command line arguments
  {match, L0} = re:run(Str, "'(.+)'|([^ ]+) *", [global, {capture, all_but_first, list}]),
  L = lists:map(fun lists:append/1, L0),
  {ok, Patch} = lee_cli:read(?MYMODEL, L),
  Patch.

%% maybe_show_help_and_exit() ->
%%   ?CFG([help])
%%     andalso open_port({spawn, "man -l docs/EMQTT\\ bench\\ daemon.man"}, [nouse_stdio, out]),
%%   emqttb:setfail(),
%%   emqttb:terminate().
