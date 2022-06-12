# EMQTT bench daemon

# Introduction

A scriptable load generator for MQTT

## Invokation

Generally speaking, this script can work in either script mode or in
deamon mode. The mode is determined by whether REST API is enabled or
not.

Basic usage: emqttb \<gloabal parameters\> @\<scenario1\> \<scenario
parameters\> \[@\<scenario2\> \<scenario parameters\> ...\]

Repeat the last run: \<code\>emqttb --again\</code\>

## Core concepts

*Worker*: a process that corresponds to a single MQTT client

*Behavior*: a callback module that defines which function worker runs in
a loop

*Group*: a group of workers with the same behavior

*Scenario*: a script that creates several worker groups and controls the
number of clients in each group using autoscale (see below)

*Stage*: scenario can be split into stages, e.g. connect clients, run
traffic, disconnect clients, etc. Behaviors can depend on the stage.

*Autorate*: a function that calculates the optimal rate value based on
some static and dynamic parameters, e.g. available RAM or CPU load.

*Autoscale*: a function that scales the size of the group up or down.

## REST API endpoints

By default, REST API is disabled and emqttb runs in script mode. To
enable it, run the script with --restapi flag.

### Methods

[/healthcheck](http://localhost:8017/healthcheck): Healthcheck endpoint.
Just returns 200 all the time.

[/metrics](http://localhost:8017/metrics): Prometheus metrics endpoiont

[/scenario/:scenario/stage](http://localhost:8017/scenario/:scenario/stage):
Returns the currently running stage of a scenario.

[/conf/reload](http://localhost:8017/conf/reload): Reload configuration
in the runtime.

# CLI arguments

## 

### \--again

Repeat the last execution., see:
[\[convenience,again\]](#[convenience,again])

### \--conf

Read configuration from a file, see: [???](#[convenience,conf_file])

### \--conf-dump-file

Name of the repeat file or \`undefined\`, see:
[\[convenience,conf\_dump\]](#[convenience,conf_dump])

### \--keep-running

Keep the process running after completing all the scenarios, see:
[\[convenience,keep\_running\]](#[convenience,keep_running])

### \--linger

Default linger time for the scenarios (sec), see:
[\[convenience,linger\]](#[convenience,linger])

### \--log-level

Global log level, see: [\[logging,level\]](#[logging,level])

### \--pushgw

Enable sending metrics to pushgateway, see:
[\[metrics,pushgateway,enabled\]](#[metrics,pushgateway,enabled])

### \--rest-listen

REST API listening interface/port, see:
[\[restapi,listen\_port\]](#[restapi,listen_port])

### \--restapi

Enable REST API, see: [\[restapi,enabled\]](#[restapi,enabled])

### \-N

Maximum number of clients used by default by all groups, see:
[\[n\_clients\]](#[n_clients])

### \-R

Default interval between events, see: [\[interval\]](#[interval])

## @g

Configuration for client groups

It is possible to override client configuration for the group.

### \--clientid

Clientid pattern, see:
[\[groups,{},client,clientid\]](#[groups,{},client,clientid])

### \--group

ID of the group, see: [\[groups,{},id\]](#[groups,{},id])

### \--host

Hostname of the target broker, see:
[\[groups,{},broker,hosts\]](#[groups,{},broker,hosts])

### \--ifaddr

Local IP addresses, see:
[\[groups,{},net,ifaddr\]](#[groups,{},net,ifaddr])

### \--lowmem

Reduce memory useage at the cost of CPU wherever possible, see:
[\[groups,{},lowmem\]](#[groups,{},lowmem])

### \--password

Password for connecting to the broker, see:
[\[groups,{},client,password\]](#[groups,{},client,password])

### \--port

Hostname of the target broker, see:
[\[groups,{},broker,port\]](#[groups,{},broker,port])

### \--session-expiry

Session expiry, see:
[\[groups,{},client,session\_expiry\]](#[groups,{},client,session_expiry])

### \--transport

Transport protocol, see:
[\[groups,{},connection,transport\]](#[groups,{},connection,transport])

### \--username

Username of the client, see:
[\[groups,{},client,username\]](#[groups,{},client,username])

### \--version

MQTT protocol version, see:
[\[groups,{},connection,proto\_ver\]](#[groups,{},connection,proto_ver])

### \-P

Password for connecting to the broker, see:
[\[groups,{},client,password\]](#[groups,{},client,password])

### \-T

Transport protocol, see:
[\[groups,{},connection,transport\]](#[groups,{},connection,transport])

### \-V

MQTT protocol version, see:
[\[groups,{},connection,proto\_ver\]](#[groups,{},connection,proto_ver])

### \-g

ID of the group, see: [\[groups,{},id\]](#[groups,{},id])

### \-h

Hostname of the target broker, see:
[\[groups,{},broker,hosts\]](#[groups,{},broker,hosts])

### \-i

Clientid pattern, see:
[\[groups,{},client,clientid\]](#[groups,{},client,clientid])

### \-p

Hostname of the target broker, see:
[\[groups,{},broker,port\]](#[groups,{},broker,port])

### \-u

Username of the client, see:
[\[groups,{},client,username\]](#[groups,{},client,username])

### \-x

Session expiry, see:
[\[groups,{},client,session\_expiry\]](#[groups,{},client,session_expiry])

## @make-docs

Run scenario make-docs

### \--linger

Keep running scenario stages for this period of time (sec), see:
[\[scenarios,emqttb\_scenario\_make\_docs,{},linger\]](#[scenarios,emqttb_scenario_make_docs,{},linger])

## @pub

Run scenario pub

### \--conninterval

Client connection interval, see:
[\[scenarios,emqttb\_scenario\_pub,{},conninterval\]](#[scenarios,emqttb_scenario_pub,{},conninterval])

### \--group

ID of the client group, see:
[\[scenarios,emqttb\_scenario\_pub,{},group\]](#[scenarios,emqttb_scenario_pub,{},group])

### \--linger

Keep running scenario stages for this period of time (sec), see:
[\[scenarios,emqttb\_scenario\_pub,{},linger\]](#[scenarios,emqttb_scenario_pub,{},linger])

### \--num-clients

Number of clients, see:
[\[scenarios,emqttb\_scenario\_pub,{},n\_clients\]](#[scenarios,emqttb_scenario_pub,{},n_clients])

### \--pubinterval

Message publishing interval, see:
[\[scenarios,emqttb\_scenario\_pub,{},pubinterval\]](#[scenarios,emqttb_scenario_pub,{},pubinterval])

### \--size

Size of the published message in bytes, see:
[\[scenarios,emqttb\_scenario\_pub,{},msg\_size\]](#[scenarios,emqttb_scenario_pub,{},msg_size])

### \--topic

Topic where the clients shall publish messages, see:
[\[scenarios,emqttb\_scenario\_pub,{},topic\]](#[scenarios,emqttb_scenario_pub,{},topic])

### \-I

Client connection interval, see:
[\[scenarios,emqttb\_scenario\_pub,{},conninterval\]](#[scenarios,emqttb_scenario_pub,{},conninterval])

### \-N

Number of clients, see:
[\[scenarios,emqttb\_scenario\_pub,{},n\_clients\]](#[scenarios,emqttb_scenario_pub,{},n_clients])

### \-g

ID of the client group, see:
[\[scenarios,emqttb\_scenario\_pub,{},group\]](#[scenarios,emqttb_scenario_pub,{},group])

### \-i

Message publishing interval, see:
[\[scenarios,emqttb\_scenario\_pub,{},pubinterval\]](#[scenarios,emqttb_scenario_pub,{},pubinterval])

### \-s

Size of the published message in bytes, see:
[\[scenarios,emqttb\_scenario\_pub,{},msg\_size\]](#[scenarios,emqttb_scenario_pub,{},msg_size])

### \-t

Topic where the clients shall publish messages, see:
[\[scenarios,emqttb\_scenario\_pub,{},topic\]](#[scenarios,emqttb_scenario_pub,{},topic])

## @sub

Run scenario sub

### \--connrate

Maximum client connection interval, see:
[\[scenarios,emqttb\_scenario\_sub,{},conninterval\]](#[scenarios,emqttb_scenario_sub,{},conninterval])

### \--linger

Keep running scenario stages for this period of time (sec), see:
[\[scenarios,emqttb\_scenario\_sub,{},linger\]](#[scenarios,emqttb_scenario_sub,{},linger])

### \--max-clients

Number of clients, see:
[\[scenarios,emqttb\_scenario\_sub,{},n\_subs\]](#[scenarios,emqttb_scenario_sub,{},n_subs])

### \--topic

Topic that the clients shall subscribe to, see:
[\[scenarios,emqttb\_scenario\_sub,{},topic\]](#[scenarios,emqttb_scenario_sub,{},topic])

### \-N

Number of clients, see:
[\[scenarios,emqttb\_scenario\_sub,{},n\_subs\]](#[scenarios,emqttb_scenario_sub,{},n_subs])

### \-r

Maximum client connection interval, see:
[\[scenarios,emqttb\_scenario\_sub,{},conninterval\]](#[scenarios,emqttb_scenario_sub,{},conninterval])

### \-t

Topic that the clients shall subscribe to, see:
[\[scenarios,emqttb\_scenario\_sub,{},topic\]](#[scenarios,emqttb_scenario_sub,{},topic])

# OS Environment Variables

The following OS environment variables are used to set configuration
values. Values of type string() are taken from OS environment variables
verbatim, other types are parsed as Erlang terms.

Priority: 0

## EMQTTB\_CONVENIENCE\_\_CONF\_DUMP

Name of the repeat file or \`undefined\`, see:
[\[convenience,conf\_dump\]](#[convenience,conf_dump])

## EMQTTB\_CONVENIENCE\_\_KEEP\_RUNNING

Keep the process running after completing all the scenarios, see:
[\[convenience,keep\_running\]](#[convenience,keep_running])

## EMQTTB\_CONVENIENCE\_\_LINGER

Default linger time for the scenarios (sec), see:
[\[convenience,linger\]](#[convenience,linger])

## EMQTTB\_LOGGING\_\_DEFAULT\_HANDLER\_LEVEL

Log level for the default handler, see:
[\[logging,default\_handler\_level\]](#[logging,default_handler_level])

## EMQTTB\_LOGGING\_\_LEVEL

Global log level, see: [\[logging,level\]](#[logging,level])

## EMQTTB\_METRICS\_\_PUSHGATEWAY\_\_ENABLED

Enable sending metrics to pushgateway, see:
[\[metrics,pushgateway,enabled\]](#[metrics,pushgateway,enabled])

## EMQTTB\_METRICS\_\_PUSHGATEWAY\_\_INTERVAL

Push interval (ms), see:
[\[metrics,pushgateway,interval\]](#[metrics,pushgateway,interval])

## EMQTTB\_METRICS\_\_PUSHGATEWAY\_\_URL

URL of pushgateway server, see:
[\[metrics,pushgateway,url\]](#[metrics,pushgateway,url])

## EMQTTB\_RESTAPI\_\_ENABLED

Enable REST API, see: [\[restapi,enabled\]](#[restapi,enabled])

## EMQTTB\_RESTAPI\_\_LISTEN\_PORT

REST API listening interface/port, see:
[\[restapi,listen\_port\]](#[restapi,listen_port])

# Configuration file /etc/emqttb/emqttb.conf

Any value can be set using this configuration file. It should have the
following form:

``` erlang
#{ key1 => value
 , key2 =>
    #{ key3 => value
     }
 }.
```

Priority: -110

# Values

This section lists all configurable values.

## \[convenience,again\]

Repeat the last execution.

*Type:*

``` erlang
boolean()
```

*Default value:*

``` erlang
false
```

*Description:*

Note: it tries best to restore the previous environment, so it only
makes sense to use this option alone, as it overrides other options.

## \[convenience,conf\_dump\]

Name of the repeat file or \`undefined\`

*Type:*

``` erlang
string() | undefined when
  char() :: 0..1114111,
  string() :: [char()].
```

*Default value:*

``` erlang
".emqttb.repeat"
```

*Description:*

If set to a string value, emqttb will dump its configuration to a
"repeat" file that can be used to quickly repeat the last run.

Note: only the successful runs of the script are saved.

## \[convenience,keep\_running\]

Keep the process running after completing all the scenarios

*Type:*

``` erlang
boolean()
```

*Default value:*

See [\[restapi,enabled\]](#[restapi,enabled])

*Description:*

By default, when started without REST API, emqttb script terminates
after completing all the scenarios, which is useful for scripting.
However, when running with REST API, such behavior is undesirable. So
when REST is enabled, the default behavior is different: the process
keeps running waiting for commands.

This flag can be used to explicitly override this behavior.

## \[convenience,linger\]

Default linger time for the scenarios (sec)

*Type:*

``` erlang
timeout() when
  non_neg_integer() :: 0..inf,
  timeout() :: non_neg_integer() | infinity.
```

*Default value:*

``` erlang
infinity
```

## \[groups,{},broker,hosts\]

Hostname of the target broker

*Type:*

``` erlang
emqttb_conf_model:hosts() when
  char() :: 0..1114111,
  emqttb_conf_model:hosts() :: [string() | {string(), emqttb_conf_model:net_port()}],
  emqttb_conf_model:net_port() :: 1..65535,
  string() :: [char()].
```

*Default value:*

``` erlang
["localhost"]
```

## \[groups,{},broker,port\]

Hostname of the target broker

*Type:*

``` erlang
emqttb_conf_model:net_port() when
  emqttb_conf_model:net_port() :: 1..65535.
```

*Default value:*

``` erlang
1883
```

## \[groups,{},client,clientid\]

Clientid pattern

*Type:*

``` erlang
binary()
```

*Default value:*

``` erlang
<<"%h-%g-%n">>
```

*Description:*

A pattern used to generate clientids. The following substitutions are
supported:

*%h*: Hostname of emqttb*%g*: Group ID*%n*: Worker number

## \[groups,{},client,password\]

Password for connecting to the broker

*Type:*

``` erlang
undefined | string() when
  char() :: 0..1114111,
  string() :: [char()].
```

*Default value:*

``` erlang
undefined
```

## \[groups,{},client,session\_expiry\]

Session expiry

*Type:*

``` erlang
non_neg_integer() when
  non_neg_integer() :: 0..inf.
```

*Default value:*

``` erlang
0
```

## \[groups,{},client,username\]

Username of the client

*Type:*

``` erlang
undefined | string() when
  char() :: 0..1114111,
  string() :: [char()].
```

*Default value:*

``` erlang
undefined
```

## \[groups,{},connection,proto\_ver\]

MQTT protocol version

*Type:*

``` erlang
emqttb:proto_ver() when
  emqttb:proto_ver() :: v5 | v4 | v3.
```

*Default value:*

``` erlang
v5
```

## \[groups,{},connection,transport\]

Transport protocol

*Type:*

``` erlang
emqttb:transport() when
  emqttb:transport() :: quic | ws | sock.
```

*Default value:*

``` erlang
sock
```

## \[groups,{},id\]

ID of the group

*Type:*

``` erlang
atom()
```

*Default value:*

``` erlang
default
```

## \[groups,{},lowmem\]

Reduce memory useage at the cost of CPU wherever possible

*Type:*

``` erlang
boolean()
```

*Default value:*

``` erlang
false
```

## \[groups,{},net,ifaddr\]

Local IP addresses

*Type:*

``` erlang
[ip_address(),...] when
  ip4_address() :: {0..255, 0..255, 0..255, 0..255},
  ip6_address() :: {0..65535, 0..65535, 0..65535, 0..65535, 0..65535, 0..65535, 0..65535, 0..65535},
  ip_address() :: ip6_address() | ip4_address().
```

*Default value:*

``` erlang
[{0,0,0,0}]
```

## \[interval\]

Default interval between events

*Type:*

``` erlang
emqttb:interval() when
  emqttb:autorate() :: atom(),
  emqttb:interval() :: {auto, emqttb:autorate()} | non_neg_integer(),
  non_neg_integer() :: 0..inf.
```

*Default value:*

``` erlang
100
```

## \[logging,default\_handler\_level\]

Log level for the default handler

*Type:*

``` erlang
lee_logger:level() when
  lee_logger:level() :: alert | critical | error | warning | notice | info | debug.
```

*Default value:*

See [\[logging,level\]](#[logging,level])

## \[logging,level\]

Global log level

*Type:*

``` erlang
lee_logger:level() when
  lee_logger:level() :: alert | critical | error | warning | notice | info | debug.
```

*Default value:*

``` erlang
notice
```

## \[metrics,pushgateway,enabled\]

Enable sending metrics to pushgateway

*Type:*

``` erlang
boolean()
```

*Default value:*

``` erlang
false
```

## \[metrics,pushgateway,interval\]

Push interval (ms)

*Type:*

``` erlang
non_neg_integer() when
  non_neg_integer() :: 0..inf.
```

*Default value:*

``` erlang
1000
```

## \[metrics,pushgateway,url\]

URL of pushgateway server

*Type:*

``` erlang
string() when
  char() :: 0..1114111,
  string() :: [char()].
```

*Default value:*

``` erlang
"http://localhost:9091"
```

## \[n\_clients\]

Maximum number of clients used by default by all groups

*Type:*

``` erlang
0..262044
```

*Default value:*

``` erlang
1000
```

## \[restapi,enabled\]

Enable REST API

*Type:*

``` erlang
boolean()
```

*Default value:*

``` erlang
false
```

## \[restapi,listen\_port\]

REST API listening interface/port

*Type:*

``` erlang
listen_port_ip4() when
  ip4_address() :: {0..255, 0..255, 0..255, 0..255},
  listen_port_ip4() :: {ip4_address(), 1..65535}.
```

*Default value:*

``` erlang
0.0.0.0:8017
```

## \[scenarios,emqttb\_scenario\_make\_docs,{},linger\]

Keep running scenario stages for this period of time (sec)

*Type:*

``` erlang
timeout() when
  non_neg_integer() :: 0..inf,
  timeout() :: non_neg_integer() | infinity.
```

*Default value:*

See [\[convenience,linger\]](#[convenience,linger])

## \[scenarios,emqttb\_scenario\_pub,{},conninterval\]

Client connection interval

*Type:*

``` erlang
emqttb:interval() when
  emqttb:autorate() :: atom(),
  emqttb:interval() :: {auto, emqttb:autorate()} | non_neg_integer(),
  non_neg_integer() :: 0..inf.
```

*Default value:*

See [\[interval\]](#[interval])

## \[scenarios,emqttb\_scenario\_pub,{},group\]

ID of the client group

*Type:*

``` erlang
atom()
```

*Default value:*

``` erlang
default
```

## \[scenarios,emqttb\_scenario\_pub,{},linger\]

Keep running scenario stages for this period of time (sec)

*Type:*

``` erlang
timeout() when
  non_neg_integer() :: 0..inf,
  timeout() :: non_neg_integer() | infinity.
```

*Default value:*

See [\[convenience,linger\]](#[convenience,linger])

## \[scenarios,emqttb\_scenario\_pub,{},msg\_size\]

Size of the published message in bytes

*Type:*

``` erlang
non_neg_integer() when
  non_neg_integer() :: 0..inf.
```

*Default value:*

``` erlang
256
```

## \[scenarios,emqttb\_scenario\_pub,{},n\_clients\]

Number of clients

*Type:*

``` erlang
0..262044
```

*Default value:*

See [\[n\_clients\]](#[n_clients])

## \[scenarios,emqttb\_scenario\_pub,{},pubinterval\]

Message publishing interval

*Type:*

``` erlang
emqttb:interval() when
  emqttb:autorate() :: atom(),
  emqttb:interval() :: {auto, emqttb:autorate()} | non_neg_integer(),
  non_neg_integer() :: 0..inf.
```

*Default value:*

See [\[interval\]](#[interval])

## \[scenarios,emqttb\_scenario\_pub,{},topic\]

Topic where the clients shall publish messages

*Type:*

``` erlang
binary()
```

## \[scenarios,emqttb\_scenario\_sub,{},conninterval\]

Maximum client connection interval

*Type:*

``` erlang
emqttb:interval() when
  emqttb:autorate() :: atom(),
  emqttb:interval() :: {auto, emqttb:autorate()} | non_neg_integer(),
  non_neg_integer() :: 0..inf.
```

*Default value:*

See [\[interval\]](#[interval])

## \[scenarios,emqttb\_scenario\_sub,{},linger\]

Keep running scenario stages for this period of time (sec)

*Type:*

``` erlang
timeout() when
  non_neg_integer() :: 0..inf,
  timeout() :: non_neg_integer() | infinity.
```

*Default value:*

See [\[convenience,linger\]](#[convenience,linger])

## \[scenarios,emqttb\_scenario\_sub,{},n\_subs\]

Number of clients

*Type:*

``` erlang
0..262044
```

*Default value:*

See [\[n\_clients\]](#[n_clients])

## \[scenarios,emqttb\_scenario\_sub,{},topic\]

Topic that the clients shall subscribe to

*Type:*

``` erlang
binary()
```
