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
-module(emqttb_conf_model).

%% API:
-export([model/0]).

-export_type([]).

-include("emqttb.hrl").
-include_lib("typerefl/include/types.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API funcions
%%================================================================================

model() ->
  #{ '$doc_root' =>
       {[doc_root],
        #{ oneliner    => "A scriptable load generator for MQTT"
         , app_name    => "EMQTT bench daemon"
         , prog_name   => "emqttb"
         }}
   , cluster =>
       #{ node_name =>
            {[value, os_env],
             #{ type     => atom()
              , default  => undefined
              }}
        }
   , interval          =>
       {[value, cli_param],
        #{ type        => emqttb:duration_us()
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
       #{ listen_port =>
            {[value, os_env, cli_param],
             #{ oneliner    => "REST API listening interface/port"
              , type        => typerefl:listen_port_ip4()
              , default_str => "0.0.0.0:8017"
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
             #{ type        => boolean()
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
             }
        }
   , convenience =>
       #{ again =>
            {[value, cli_param],
             #{ oneliner    => "Repeat the last execution"
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
       {[map, cli_action, default_instance],
        #{ cli_operand  => "g"
         , key_elements => [[id]]
         },
        emqttb_worker:model()}
   , autorate =>
       {[map, cli_action, default_instance],
        #{ cli_operand  => "a"
         , key_elements => [[id]]
         },
        emqttb_autorate:model()}
   }.

%%================================================================================
%% Internal functions
%%================================================================================
