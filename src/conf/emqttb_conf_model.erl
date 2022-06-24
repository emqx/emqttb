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
-module(emqttb_conf_model).

%% API:
-export([model/0]).

%% internal exports:
-export([parse_hosts/1, parse_addresses/1]).

-export_type([]).

-include("emqttb.hrl").
-include_lib("typerefl/include/types.hrl").

-import(lee_doc, [sect/3, p/1, li/2]).

%%================================================================================
%% Type declarations
%%================================================================================

-type net_port() :: 1..65535.

-type hosts() :: [{string(), net_port()} | string()].

-reflect_type([net_port/0, hosts/0]).

%%================================================================================
%% API funcions
%%================================================================================

model() ->
  #{ '$doc_root' =>
       {[doc_root],
        #{ oneliner    => "A scriptable load generator for MQTT"
         , app_name    => "EMQTT bench daemon"
         , doc         => intro()
         , prog_name   => "emqttb"
         }}
   , interval          =>
       {[value, cli_param],
        #{ oneliner    => "Default interval between events"
         , type        => emqttb:interval()
         , default     => 100
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
        , loiter =>
            {[value, os_env, cli_param],
             #{ oneliner    => "Default loiter time for the scenarios (sec)"
              , type        => timeout()
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
        #{ oneliner     => "Configuration for client groups"
         , doc          => "<para>
                              It is possible to override client configuration for the group.
                            </para>"
         , cli_operand  => "g"
         , key_elements => [[id]]
         },
        group_model()}
   , autorate =>
       {[map, cli_action, default_instance],
        #{ oneliner     => "Autorate configuration"
         , cli_operand  => "a"
         , key_elements => [[id]]
         },
       autorate_model()}
   }.

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================

group_model() ->
  #{ id =>
       {[value, cli_param],
        #{ oneliner    => "ID of the group"
         , type        => atom()
         , default     => default
         , cli_operand => "group"
         , cli_short   => $g
         }}
   , lowmem =>
       {[value, cli_param],
        #{ oneliner    => "Reduce memory useage at the cost of CPU wherever possible"
         , type        => boolean()
         , default     => false
         , cli_operand => "lowmem"
         }}
   , broker =>
       #{ hosts =>
            {[value, cli_param],
             #{ oneliner    => "Hostname of the target broker"
              , type        => hosts()
              , default     => ["localhost"]
              , cli_operand => "host"
              , cli_short   => $h
              , from_string => fun parse_hosts/1
              }}
        , port =>
            {[value, cli_param],
             #{ oneliner    => "Hostname of the target broker"
              , type        => union(net_port(), default)
              , default     => default
              , cli_operand => "port"
              , cli_short   => $p
              }}
        }
   , connection =>
       #{ proto_ver =>
            {[value, cli_param],
             #{ oneliner    => "MQTT protocol version"
              , type        => emqttb:proto_ver()
              , default     => v5
              , cli_operand => "version"
              , cli_short   => $V
              }}
        , transport =>
            {[value, cli_param],
             #{ oneliner    => "Transport protocol"
              , type        => emqttb:transport()
              , default     => sock
              , cli_operand => "transport"
              , cli_short   => $T
              }}
        , inflight =>
            {[value, cli_param],
             #{ oneliner    => "maximum inflight messages for QoS 1 and 2"
              , type        => union(non_neg_integer(), infinity)
              , default     => 10
              , cli_operand => "inflight"
              , cli_short   => $F
              }}
        }
   , client =>
       #{ clientid =>
            {[value, cli_param],
             #{ oneliner    => "Clientid pattern"
              , doc         => [ p("A pattern used to generate clientids.
                                   The following substitutions are supported:")
                               , {itemlist,
                                  [ li("%h", ["Hostname of emqttb"])
                                  , li("%g", ["Group ID"])
                                  , li("%n", ["Worker number"])
                                  ]}
                               ]
              , type        => binary()
              , default     => <<"%h-%g-%n">>
              , cli_operand => "clientid"
              , cli_short   => $i
              }}
        , username =>
            {[value, cli_param],
             #{ oneliner    => "Username of the client"
              , type        => union(undefined, string())
              , default     => undefined
              , cli_operand => "username"
              , cli_short   => $u
              }}
        , password =>
            {[value, cli_param],
             #{ oneliner    => "Password for connecting to the broker"
              , type        => union(undefined, string())
              , default     => undefined
              , cli_operand => "password"
              , cli_short   => $P
              }}
        }
   , net =>
       #{ ifaddr =>
            {[value, cli_param],
             #{ oneliner    => "Local IP addresses"
              , type        => nonempty_list(typerefl:ip_address())
              , default     => [{0, 0, 0, 0}]
              , from_string => fun parse_addresses/1
              , cli_operand => "ifaddr"
              }}
        }
   , ssl =>
       #{ enable =>
            {[value, cli_param],
             #{ oneliner    => "Enable SSL for the connections"
              , type        => boolean()
              , default     => false
              , cli_operand => "ssl"
              }}
        , certfile =>
            {[value, cli_param],
             #{ oneliner    => "Client certificate for authentication, if required by the server"
              , type        => string()
              , default     => ""
              , cli_operand => "certfile"
              }}
        , keyfile =>
            {[value, cli_param],
             #{ oneliner    => "Client private key for authentication, if required by the server"
              , type        => string()
              , default     => ""
              , cli_operand => "keyfile"
              }}
        }
   }.

autorate_model() ->
  #{ id =>
       {[value, cli_param],
        #{ oneliner  => "ID of the autorate configuration"
         , type      => atom()
         , default   => default
         , cli_param => "autorate"
         , cli_short => $a
         }}
   , min =>
       {[value, cli_param],
        #{ oneliner  => "Minimum value of the controlled parameter"
         , type      => integer()
         , default   => 0
         , cli_param => "min"
         , cli_short => $m
         }}
   , max =>
       {[value, cli_param],
        #{ oneliner  => "Maximum value of the controlled parameter"
         , type      => integer()
         , default   => 100000
         , cli_param => "max"
         , cli_short => $M
         }}
   , speed =>
       {[value, cli_param],
        #{ oneliner  => "Maximum rate of change of the controlled parameter"
         , type      => integer()
         , default   => 0
         , cli_param => "speed"
         , cli_short => $V
         }}
   , k_p =>
       {[value, cli_param],
        #{ oneliner  => "Controller gain"
         , type      => number()
         , default   => 0.00005
         , cli_param => "Kp"
         , cli_short => $p
         }}
   , t_i =>
       {[value, cli_param],
        #{ oneliner  => "Controller reset time"
         , type      => number()
         , default   => 1
         , cli_param => "Ti"
         , cli_short => $I
         }}
   }.

intro() ->
  [ sect("intro-running", "Invokation",
         [ p("Generally speaking, this script can work in either script mode or in deamon mode.
              The mode is determined by whether REST API is enabled or not.")
         , p("Basic usage: emqttb <gloabal parameters> @<scenario1> <scenario parameters> [@<scenario2> <scenario parameters> ...]")
         , p("Repeat the last run: <code>emqttb --again</code>")
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

parse_addresses(Str) ->
  L = [inet:parse_address(I) || I <- string:tokens(Str, ", ")],
  case lists:keyfind(error, 1, L) of
    false ->
      {ok, [I || {ok, I} <- L]};
    _ ->
      error
  end.

parse_hosts(Str) ->
  try
    {ok, [parse_host(I) || I <- string:tokens(Str, ", ")]}
  catch
    _:_ ->
      error
  end.

parse_host(Str) ->
  case string:tokens(Str, ":") of
    [Host] ->
      Host;
    [Host, Port] ->
      {Host, list_to_integer(Port)}
  end.
