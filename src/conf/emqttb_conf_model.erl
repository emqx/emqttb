%%--------------------------------------------------------------------
%% Copyright (c) 2022-2023, 2025 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(emqttb_conf_model).

%% API:
-export([model/0]).

-export_type([object_type/0]).

-include("emqttb.hrl").
-include_lib("typerefl/include/types.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type object_type() :: autorate | metric.
-reflect_type([object_type/0]).

%%================================================================================
%% API funcions
%%================================================================================

model() ->
  #{ cluster =>
       #{ node_name =>
            {[value, os_env],
             #{ oneliner => "Node name"
              , doc      => "Note: erlang distribution is disabled when node name is @code{undefined}.\n"
              , type     => atom()
              , default  => undefined
              }}
        }
   , interval          =>
       {[value, cli_param],
        #{ oneliner    => "Default interval between events"
         , doc         => "@doc-interval"
         , type        => emqttb:duration_us()
         , default_str => "10ms"
         , cli_operand => "max-rate"
         , cli_short   => $R
         }}
   , n_clients         =>
       {[value, cli_param],
        #{ oneliner    => "Maximum number of clients used by default by all groups"
         , type        => emqttb:n_clients()
         , default     => 1000
         , cli_operand => "max-clients"
         , cli_short   => $N
         }}
   , inet =>
       #{ reuseaddr =>
            {[value, os_env],
             #{ oneliner => "Enable SO_REUSEADDR option for the TCP sockets"
              , type     => boolean()
              , default  => true
              }}
        }
   , restapi =>
       #{ enabled =>
            {[value, os_env, cli_param],
             #{ oneliner    => "Enable REST API"
              , doc         => "@option{--restapi} CLI argument enables REST API
                                (available at @url{http://127.0.0.0:8017} by default),
                                and it also means that the script keeps running after completing the scenarios.
                                "
              , type        => boolean()
              , default     => false
              , cli_operand => "restapi"
              }}
        , listen_port =>
            {[value, os_env, cli_param],
             #{ oneliner    => "REST API listening interface/port"
              , type        => typerefl:listen_port_ip4()
              , default_str => "0.0.0.0:8017"
              , cli_operand => "rest-listen"
              }}
        , tls =>
            {[value, os_env],
             #{ oneliner => "Enable TLS for REST listener"
              , type     => boolean()
              , default  => false
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
        , client_handler_level =>
            {[value, os_env, logger_level],
             #{ oneliner       => "Log level for the MQTT clients and workers"
              , type           => lee_logger:level()
              , logger_handler => client
              , default_ref    => [logging, level]
              }}
        , directory =>
            {[value, os_env],
             #{ oneliner       => "Directory for the logs"
              , type           => string()
              , default        => "/tmp/"
              }}
        }
   , metrics =>
       #{ pushgateway =>
            #{ url =>
                 {[value, cli_param, os_env],
                  #{ oneliner    => "URL of pushgateway server"
                   , type        => string()
                   , default     => "http://localhost:9091"
                   , cli_operand => "pushgw-url"
                   }}
             , enabled           =>
                 {[value, cli_param, os_env],
                  #{ oneliner    => "Enable sending metrics to pushgateway"
                   , type        => boolean()
                   , default     => false
                   , cli_operand => "pushgw"
                   }}
             , interval          =>
                 {[value, cli_param, os_env],
                  #{ oneliner    => "Push interval (ms)"
                   , type        => emqttb:duration_ms()
                   , default_str => "1s"
                   , cli_operand => "pushgw-interval"
                   }}
             }
        , grafana =>
            #{ url =>
                 {[value, cli_param,  os_env],
                  #{ oneliner    => "URL of Grafana server"
                   , type        => string()
                   , default     => "http://localhost:3000"
                   , cli_operand => "grafana-url"
                   }}
             , enabled =>
                 {[value, cli_param, os_env],
                  #{ oneliner    => "Add annotations to Grafana"
                   , type        => boolean()
                   , default     => false
                   , cli_operand => "grafana"
                   }}
             , login =>
                 {[value, cli_param, os_env],
                  #{ oneliner    => "Grafana login"
                   , type        => union(false, string())
                   , default     => false
                   , cli_operand => "grafana-login"
                   }}
             , password =>
                 {[value, os_env],
                  #{ oneliner    => "Grafana password"
                   , type        => string()
                   , default     => ""
                   }}
             , api_key =>
                 {[value, os_env],
                  #{ oneliner    => "Grafana API key"
                   , type        => union(false, string())
                   , default     => false
                   }}
             , motto =>
                 {[value, os_env, cli_param],
                  #{ oneliner    => "Add Grafana annotation at the end of the run"
                   , type        => string()
                   , default     => ""
                   , cli_operand => "motto"
                   }}
             , motto_tags =>
                 {[value, os_env, cli_param],
                  #{ oneliner    => "Custom tags added to the Grafana annotation span"
                   , type        => emqttb_grafana:tags()
                   , default     => []
                   , cli_operand => "motto-tags"
                   }}
             }
        }
   , convenience =>
       #{ again =>
            {[value, cli_param],
             #{ oneliner    => "Repeat the last execution"
              , doc         => "Note: it tries best to restore the previous environment,
                                so it only makes sense to use this option alone, as
                                it overrides other options.
                                "
              , type        => boolean()
              , default     => false
              , cli_operand => "again"
              }}
        , conf_dump =>
            {[value, os_env, cli_param],
             #{ oneliner    => "Name of the repeat file or `undefined`"
              , doc         => "If set to a string value, emqttb will dump its configuration
                                to a \"repeat\" file that can be used to quickly repeat the last run.

                                Note: only the successful runs of the script are saved.
                                "
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
        , loiter =>
            {[value, os_env, cli_param],
             #{ oneliner    => "Default loiter time for the scenarios (sec)"
              , type        => emqttb:wait_time()
              , default     => infinity
              , cli_operand => "loiter"
              , cli_short   => $L
              }}
        , keep_running =>
            {[value, os_env, cli_param],
             #{ oneliner => "Keep the process running after completing all the scenarios"
              , doc => "By default, when started without REST API, emqttb script terminates
                        after completing all the scenarios, which is useful for scripting.
                        However, when running with REST API, such behavior is undesirable.
                        So when REST is enabled, the default behavior is different: the
                        process keeps running waiting for commands.

                        This flag can be used to explicitly override this behavior.
                        "
              , type => boolean()
              , default_ref => [restapi, enabled]
              , cli_operand => "keep-running"
              }}
        }
   , scenarios => emqttb_scenario:model()
   , actions =>
       #{ ls =>
            {[map, cli_action],
             #{ cli_operand => "ls"
              , key_elements => []
              , oneliner => "List objects and exit"
              },
             #{ what =>
                  {[value, cli_positional],
                   #{ oneliner => "Type of objects to list"
                    , type => object_type()
                    , cli_arg_position => 1
                    }}
              }}
        }
   , groups =>
       {[map, cli_action, default_instance],
        #{ oneliner     => "Client configuration"
         , doc          => "Client configuration is kept separate from the scenario config.
                            This is done so scenarios could share client configuration.
                            "
         , cli_operand  => "g"
         , key_elements => [[id]]
         },
        emqttb_worker:model()}
   , autorate =>
       {[map, cli_action],
        #{ oneliner     => "Autorate configuration"
         , doc          => "@doc-autorate"
         , cli_operand  => "a"
         , key_elements => [[id]]
         },
        emqttb_autorate:model()}
   }.

%%================================================================================
%% Internal functions
%%================================================================================
