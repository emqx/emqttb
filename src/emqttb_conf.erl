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
-module(emqttb_conf).

%% API:
-export([load_conf/1, maybe_dump_conf/0, get/1, list_keys/1, reload/0, patch/1]).

-export_type([]).

-include("emqttb.hrl").
-include_lib("typerefl/include/types.hrl").

-import(lee_doc, [sect/3, p/1, li/2]).

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API funcions
%%================================================================================

load_conf(CLIArgs) ->
  Storage = lee_storage:new(lee_persistent_term_storage),
  MTs = [ lee:base_metamodel()
        , lee_metatype:create(lee_os_env, #{prefix => "EMQTTB_", priority => 0})
        , lee_metatype:create(lee_app_env)
        , lee_metatype:create(lee_logger)
        , lee_metatype:create(lee_cli, #{cli_opts => CLIArgs, priority => 10})
        , lee_metatype:create(lee_config_file, #{ tag => global_conf
                                                , priority => -100
                                                , file => "~/.config/emqttb.eterm"
                                                })
        , lee_metatype:create(emqttb_scenario)
        ],
  {ok, Model} = lee_model:compile(MTs, [model()]),
  lee_doc:make_docs(Model, #{metatypes => [cli_param, os_env, global_conf, value], run_pandoc => true}),
  case lee:init_config(Model, Storage) of
    {ok, _Data, _Warnings} ->
      persistent_term:put(?CONF_STORE, Storage),
      persistent_term:put(?MODEL_STORE, Model),
      maybe_load_repeat(),
      ok;
    {error, Errors, _Warnings} ->
      [logger:critical(E) || E <- Errors],
      emqttb:terminate(nok)
  end.

reload() ->
  logger:notice("Reloading configuration"),
  case lee:init_config(persistent_term:get(?MODEL_STORE), persistent_term:get(?CONF_STORE)) of
    {ok, _, _} -> true;
    _          -> false
  end.

patch(Patch) ->
  logger:notice("Patching configuration: ~p", [Patch]),
  case lee:patch(persistent_term:get(?MODEL_STORE), persistent_term:get(?CONF_STORE), Patch) of
    {ok, _, _} -> true;
    _          -> false
  end.


maybe_dump_conf() ->
  case {?CFG([convenience, dump_conf]), ?CFG([convenience, again])} of
    {Filename, false} when is_list(Filename) ->
      Dump = lee_storage:dump(persistent_term:get(?CONF_STORE)),
      {ok, FD} = file:open(Filename, [write]),
      [ok = io:format(FD, "~p.~n", [I]) || I <- Dump],
      ok = file:close(FD);
    _ ->
      ok
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

maybe_load_repeat() ->
  case {?CFG([convenience, dump_conf]), ?CFG([convenience, again])} of
    {Filename, true} when is_list(Filename) ->
      case file:consult(Filename) of
        {ok, Patch} ->
          lee:patch( persistent_term:get(?MODEL_STORE)
                   , persistent_term:get(?CONF_STORE)
                   , Patch
                   );
        _ ->
          ok
      end;
    _ ->
      ok
  end.

model() ->
  #{ '$doc_root' =>
       {[doc_root],
        #{ oneliner => "A scriptable load generator for MQTT"
         , app_name => "EMQTT bench daemon"
         , doc => intro()
         , prog_name => "emqttb"
         }}
   %% , bootstrap_node =>
   %%     {[value, os_env, cli_param],
   %%      #{ oneliner    => "Connect to this node to form the cluster"
   %%       , doc         => "<para>By default set to <code language=\"erlang\">node()</code>,
   %%                         which is equivalent to no clustering.</para>"
   %%       , type        => typerefl:node()
   %%       , default     => node()
   %%       , cli_operand => "bootstrap-node"
   %%       , cli_short   => $B
   %%       }}
   , rate =>
       {[value, cli_param],
        #{ oneliner  => "Default max rate used by all groups (events/sec)"
         , type      => emqttb:rate()
         , default   => 10.0
         , cli_param => "max-rate"
         , cli_short => $R
         }}
   , n_clients =>
       {[value, cli_param],
        #{ oneliner  => "Default maximum number of clients used by all groups"
         , type      => non_neg_integer()
         , default   => 1000
         , cli_param => "max-clients"
         , cli_short => $N
         }}
   , broker =>
       #{ host =>
            {[value, cli_param],
             #{ oneliner    => "Hostname of the target"
              , doc         => "<para>Note, this parameter can be overriden per group</para>"
              , type        => nonempty_list(typerefl:listen_port_ip4())
              , default     => [{{127, 0, 0, 1}, 1883}]
              , cli_operand => "host"
              , cli_short   => $h
              }}
        }
   , rest =>
       #{ listen_port =>
            {[value, os_env, cli_param],
             #{ oneliner    => "REST API listening interface/port"
              , type        => typerefl:listen_port_ip4()
              , default     => {{0, 0, 0, 0}, ?DEFAULT_PORT}
              , cli_operand => "rest-listen"
              }}
        , tls =>
            {[value, os_env, undocumented], % TODO
             #{ oneliner => "Enable TLS for REST listener"
              , type     => boolean()
              , default  => false
              }}
        , enabled =>
            {[value, os_env, cli_param],
             #{ oneliner    => "Enable REST API"
              , type        => boolean()
              , default     => false
              , cli_operand => "restapi"
              }}
        }
   , logging =>
       #{ level =>
            {[value, os_env, cli_param, logger_level],
             #{ oneliner    => "Global log level"
              , type        => lee_logger:level()
              , default     => notice
              , cli_operand => "log-level"
              }}
        , default_handler_level =>
            {[value, os_env, logger_level],
             #{ oneliner       => "Log level for the default handler"
              , type           => lee_logger:level()
              , default_ref    => [logging, level]
              , logger_handler => default
              }}
        }
   , metrics =>
       #{ pushgateway =>
            #{ address =>
                 {[value, os_env],
                  #{ oneliner => "URL of pushgateway server"
                   , type => string()
                   , default => "http://localhost:9091"
                   }}
             , enabled =>
                 {[value, os_env],
                  #{ oneliner => "Enable sending metrics to pushgateway"
                   , type => boolean()
                   , default => false
                   }}
             , interval =>
                 {[value, os_env],
                  #{ oneliner => "Push interval (ms)"
                   , type => non_neg_integer()
                   , default => timer:seconds(1)
                   }}
             }
        }
   , convenience =>
       #{ again =>
            {[value, cli_param],
             #{ oneliner    => "Repeat the last execution."
              , doc         => "<para>
                                  Note: it tries best to restore the previous environment,
                                  so it only makes sense to use this option alone, as
                                  it overrides other options.
                                </para>"
              , type        => boolean()
              , default     => false
              , cli_operand => "again"
              }}
        , dump_conf =>
            {[value, os_env, cli_param],
             #{ oneliner    => "Name of the repeat file or `undefined`"
              , doc         => "<para>
                                  If set to a string value, emqttb will dump its configuration
                                 to a \"repeat\" file that can be used to quickly repeat the last run.
                               </para>
                               <para>
                                 Note: only the successful runs of the script are saved.
                               </para>"
              , type        => union([undefined, string()])
              , default     => ".emqttb.repeat"
              , cli_operand => "conf-dump-file"
              }}
        , linger =>
            {[value, cli_param],
             #{ oneliner    => "Keep running after completing the scenario"
              , type        => timeout()
              , default     => infinity
              , cli_operand => "linger"
              }}
        }
   , scenarios => emqttb_scenario:model()
   }.

intro() ->
  [ sect("intro-running", "Invokation",
         [ p("Basic usage: emqttb <gloabal parameters> @<scenario1> <scenario parameters> [@<scenario2> <scenario parameters> ...]")
         , p("Repeat the last run: emqttb --again")
         ])
  , sect("concepts", "Core concepts",
         [{itemlist,
           [ li("Worker",
                ["a process that corresponds to a single MQTT client"])
           , li("Behavior",
                ["a callback module that defines which function worker runs in a loop"])
           , li("Group",
                ["a group of workers with the same behavior"])
           , li("Scenario",
                ["a script that creates several worker groups and controls the number
                  of clients in each group using autoscale (see below)"])
           , li("Stage",
                ["scenario can be split into stages, e.g. connect clients, run traffic, disconnect clients, etc.
                  Behaviors can depend on the stage."])
           , li("Autorate",
                ["a function that calculates the optimal rate value based on some
                  static and dynamic parameters, e.g. available RAM or CPU load."])
           , li("Autoscale",
                ["a function that scales the size of the group up or down."])
           ]}])
  , sect("rest", "REST API endpoints",
         [{itemlist,
           emqttb_http:doc()}])
  ].
