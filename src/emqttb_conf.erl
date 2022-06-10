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
-export([load_conf/0, get/1, list_keys/1, reload/0, patch/1]).

-export_type([]).

-include("emqttb.hrl").
-include_lib("lee/include/lee.hrl").
-include_lib("typerefl/include/types.hrl").

-import(lee_doc, [sect/3, p/1, li/2]).

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API funcions
%%================================================================================

load_conf() ->
  Storage = lee_storage:new(lee_persistent_term_storage),
  MTs = metamodel(),
  {ok, Model} = lee_model:compile(MTs, [model()]),
  persistent_term:put(?CONF_STORE, Storage),
  persistent_term:put(?MODEL_STORE, Model),
  case lee:init_config(Model, Storage) of
    {ok, _Data, _Warnings} ->
      maybe_load_repeat(),
      maybe_load_conf_file(),
      maybe_dump_conf(),
      ok;
    {error, Errors, _Warnings} ->
      [logger:critical(E) || E <- Errors],
      emqttb:setfail(),
      emqttb:terminate()
  end.

reload() ->
  logger:notice("Reloading configuration"),
  case lee:init_config(?MYMODEL, ?MYCONF) of
    {ok, _, _} -> true;
    _          -> false
  end.

patch(Patch) ->
  logger:notice("Patching configuration: ~p", [Patch]),
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

%% maybe_show_help_and_exit() ->
%%   ?CFG([help])
%%     andalso open_port({spawn, "man -l docs/EMQTT\\ bench\\ daemon.man"}, [nouse_stdio, out]),
%%   emqttb:setfail(),
%%   emqttb:terminate().

model() ->
  #{ '$doc_root' =>
       {[doc_root],
        #{ oneliner  => "A scriptable load generator for MQTT"
         , app_name  => "EMQTT bench daemon"
         , doc       => intro()
         , prog_name => "emqttb"
         }}
   %% , help =>
   %%     {[value, cli_param, undocumented],
   %%      #{ oneliner    => "Show help and exit"
   %%       , type        => boolean()
   %%       , default     => false
   %%       , cli_operand => "help"
   %%       }}
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
         , default   => 10
         , cli_param => "max-rate"
         , cli_short => $R
         }}
   , n_clients =>
       {[value, cli_param],
        #{ oneliner  => "Default maximum number of clients used by all groups"
         , type      => emqttb:n_clients()
         , default   => 1000
         , cli_param => "max-clients"
         , cli_short => $N
         }}
     %% Default clients' connection settings:
   , client => default_client_model()
   , restapi =>
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
            #{ url =>
                 {[value, os_env],
                  #{ oneliner    => "URL of pushgateway server"
                   , type        => string()
                   , default     => "http://localhost:9091"
                   }}
             , enabled           =>
                 {[value, cli_param, os_env],
                  #{ oneliner    => "Enable sending metrics to pushgateway"
                   , type        => boolean()
                   , default     => false
                   , cli_operand => "pushgw"
                   }}
             , interval          =>
                 {[value, os_env],
                  #{ oneliner    => "Push interval (ms)"
                   , type        => non_neg_integer()
                   , default     => timer:seconds(1)
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
        , conf_dump =>
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
        , conf_file =>
            {[value, cli_param, undocumented], %% Currently scuffed
             #{ oneliner    => "Read configuration from a file"
              , type        => union(string(), undefined)
              , default     => undefined
              , cli_operand => "conf"
              }}
        , linger =>
            {[value, os_env, cli_param],
             #{ oneliner    => "Default linger time for the scenarios (sec)"
              , type        => timeout()
              , default     => infinity
              , cli_operand => "linger"
              }}
        , keep_running =>
            {[value, os_env, cli_param],
             #{ oneliner => "Keep the process running after completing all the scenarios"
              , doc => "<para>
                          By default, when started without REST API, emqttb script terminates
                          after completing all the scenarios, which is useful for scripting.
                          However, when running with REST API, such behavior is undesirable.
                          So when REST is enabled, the default behavior is different: the
                          process keeps running waiting for commands.
                        </para>
                        <para>
                          This flag can be used to explicitly override this behavior.
                        </para>"
              , type => boolean()
              , default_ref => [restapi, enabled]
              , cli_operand => "keep-running"
              }}
        }
   , scenarios => emqttb_scenario:model()
   , groups =>
       {[map, cli_action],
        #{ oneliner     => "Configuration for client groups"
         , doc          => "<para>
                              It is possible to override client configuration for the group.
                            </para>"
         , cli_operand  => "group-cfg"
         , key_elements => [[id]]
         },
        group_model()}
   }.

group_model() ->
  maps:merge(
    #{ id =>
         {[value, cli_param],
          #{ oneliner    => "ID of the group"
           , type        => atom()
           , cli_operand => "id"
           , cli_short   => $i
           }}
     },
    client_model()
   ).

client_model() ->
  Fun = fun(Key, MNode = {MTs, MPs}) ->
            { MTs -- [os_env]
            , (maps:without([default], MPs)) #{default_ref => [client|Key]}
            }
        end,
  lee_model:map_vals(Fun, default_client_model()).

default_client_model() ->
  #{ host =>
       {[value, os_env, cli_param],
        #{ oneliner    => "Hostname of the target broker"
         , type        => nonempty_string()
         , default     => "localhost"
         , cli_operand => "host"
         , cli_short   => $h
         }}
   , port =>
       {[value, os_env, cli_param],
        #{ oneliner    => "Hostname of the target broker"
         , type        => range(1, 1 bsl 16 - 1)
         , default     => 1883
         , cli_operand => "port"
         , cli_short   => $p
         }}
   , version =>
       {[value, os_env, cli_param],
        #{ oneliner    => "MQTT protocol version"
         , type        => range(1, 5)
         , default     => 5
         , cli_operand => "version"
         , cli_short   => $V
         }}
   , username =>
       {[value, os_env, cli_param],
        #{ oneliner    => "Username of the client"
         , type        => union(string(), undefined)
         , default     => undefined
         , cli_operand => "username"
         , cli_short   => $u
         }}
   , password =>
       {[value, os_env, cli_param],
        #{ oneliner    => "Password for connecting to the broker"
         , type        => union(string(), undefined)
         , default     => undefined
         , cli_operand => "password"
         , cli_short   => $P
         }}
   , session_expiry =>
       {[value, os_env, cli_param],
        #{ oneliner    => "Session expiry"
         , type        => non_neg_integer()
         , default     => 0
         , cli_operand => "session-expiry"
         , cli_short   => $x
         }}
   , ifaddr =>
       {[value, cli_param],
        #{ oneliner    => "Local IP addresses"
         , type        => nonempty_list(typerefl:ip_address())
         , default     => [{0, 0, 0, 0}]
         , cli_operand => "ifaddr"
         }}
   }.

intro() ->
  [ sect("intro-running", "Invokation",
         [ p("Generally speaking, this script can work in either script mode or in deamon mode.
              The mode is determined by whether REST API is enabled or not.")
         , p("Basic usage: emqttb <gloabal parameters> @<scenario1> <scenario parameters> [@<scenario2> <scenario parameters> ...]")
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
         [ p("By default, REST API is disabled and emqttb runs in script mode.
              To enable it, run the script with --restapi flag.")
         , sect("rest-methods", "Methods",
                [{itemlist,
                  emqttb_http:doc()}])
         ])
  ].

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
  ].

cli_args_getter() ->
  application:get_env(emqttb, cli_args, []).
