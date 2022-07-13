# EMQTT bench daemon

# Introduction

A scriptable load generator for MQTT

*Description:*

## Invokation

Generally speaking, this script can work in either script mode or in
deamon mode. The mode is determined by whether REST API is enabled or
not.

Basic usage: emqttb \<gloabal parameters\> @\<scenario1\> \<scenario
parameters\> \[@\<scenario2\> \<scenario parameters\> ...\]

Repeat the last run: emqttb --again

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

# CLI arguments

## 

### \--again

Repeat the last execution., see: [convenience/again](#convenience/again)

### \--conf-dump-file

Name of the repeat file or \`undefined\`, see:
[convenience/conf\_dump](#convenience/conf_dump)

### \--conf

Read configuration from a file, see: [???](#convenience/conf_file)

### \--keep-running

Keep the process running after completing all the scenarios, see:
[convenience/keep\_running](#convenience/keep_running)

### \--loiter, -L

Default loiter time for the scenarios (sec), see:
[convenience/loiter](#convenience/loiter)

### \--max-rate, -R

Default interval between events, see: [interval](#interval)

### \--log-level

Global log level, see: [logging/level](#logging/level)

### \--pushgw

Enable sending metrics to pushgateway, see:
[metrics/pushgateway/enabled](#metrics/pushgateway/enabled)

### \--max-clients, -N

Maximum number of clients used by default by all groups, see:
[n\_clients](#n_clients)

### \--restapi

Enable REST API, see: [restapi/enabled](#restapi/enabled)

### \--rest-listen

REST API listening interface/port, see:
[restapi/listen\_port](#restapi/listen_port)

## @a

Autorate configuration

When the loadgen creates too much traffic, the system may get
overloaded. In this case, the test usually has to be restarted all over
again with different parameters. This can be very expensive in man-hours
and computing resources.

In order to prevent that, emqttb can tune some parameters (such as
message publishing interval) automatically using [PI
contoller](https://controlguru.com/integral-reset-windup-jacketing-logic-and-the-velocity-pi-form/)

### CLI arguments

#### \--autorate, -a

ID of the autorate configuration, see: [autorate/{}/id](#autorate/{}/id)

#### \--Kp, -p

Controller gain, see: [autorate/{}/k\_p](#autorate/{}/k_p)

#### \--max, -M

Maximum value of the controlled parameter, see:
[autorate/{}/max](#autorate/{}/max)

#### \--min, -m

Minimum value of the controlled parameter, see:
[autorate/{}/min](#autorate/{}/min)

#### \--speed, -V

Maximum rate of change of the controlled parameter, see:
[autorate/{}/speed](#autorate/{}/speed)

#### \--Ti, -I

Controller reset time, see: [autorate/{}/t\_i](#autorate/{}/t_i)

## @g

Configuration for client groups

*Description:*

It is possible to override client configuration for the group.

### CLI arguments

#### \--autoscale, -A

Pointer at autorate configuration, see:
[groups/{}/autoscale](#groups/{}/autoscale)

#### \--host, -h

Hostname of the target broker, see:
[groups/{}/broker/hosts](#groups/{}/broker/hosts)

#### \--port, -p

Port of the target broker, see:
[groups/{}/broker/port](#groups/{}/broker/port)

#### \--clientid, -i

Clientid pattern, see:
[groups/{}/client/clientid](#groups/{}/client/clientid)

#### \--password, -P

Password for connecting to the broker, see:
[groups/{}/client/password](#groups/{}/client/password)

#### \--username, -u

Username of the client, see:
[groups/{}/client/username](#groups/{}/client/username)

#### \--inflight, -F

maximum inflight messages for QoS 1 and 2, see:
[groups/{}/connection/inflight](#groups/{}/connection/inflight)

#### \--version, -V

MQTT protocol version, see:
[groups/{}/connection/proto\_ver](#groups/{}/connection/proto_ver)

#### \--transport, -T

Transport protocol, see:
[groups/{}/connection/transport](#groups/{}/connection/transport)

#### \--group, -g

ID of the group, see: [groups/{}/id](#groups/{}/id)

#### \--lowmem

Reduce memory useage at the cost of CPU wherever possible, see:
[groups/{}/lowmem](#groups/{}/lowmem)

#### \--ifaddr

Local IP addresses, see: [groups/{}/net/ifaddr](#groups/{}/net/ifaddr)

#### \--certfile

Client certificate for authentication, if required by the server, see:
[groups/{}/ssl/certfile](#groups/{}/ssl/certfile)

#### \--ssl

Enable SSL for the connections, see:
[groups/{}/ssl/enable](#groups/{}/ssl/enable)

#### \--keyfile

Client private key for authentication, if required by the server, see:
[groups/{}/ssl/keyfile](#groups/{}/ssl/keyfile)

## @make-docs

Run scenario make-docs

### CLI arguments

#### \--src

Path to the external documentation source file, see:
[scenarios/emqttb\_scenario\_make\_docs/{}/external\_doc](#scenarios/emqttb_scenario_make_docs/{}/external_doc)

#### \--loiter

Keep running scenario stages for this period of time (sec), see:
[scenarios/emqttb\_scenario\_make\_docs/{}/loiter](#scenarios/emqttb_scenario_make_docs/{}/loiter)

## @persistent\_session

Run scenario persistent\_session

### CLI arguments

#### \--conninterval, -I

Client connection interval (microsecond), see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/conninterval](#scenarios/emqttb_scenario_persistent_session/{}/conninterval)

#### \--group, -g

ID of the client group, see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/group](#scenarios/emqttb_scenario_persistent_session/{}/group)

#### \--loiter

Keep running scenario stages for this period of time (sec), see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/loiter](#scenarios/emqttb_scenario_persistent_session/{}/loiter)

#### \--cycles, -C

How many times to repeat publish/consume cycle, see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/n\_cycles](#scenarios/emqttb_scenario_persistent_session/{}/n_cycles)

#### \--size, -s

Size of the published message in bytes, see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/pub/msg\_size](#scenarios/emqttb_scenario_persistent_session/{}/pub/msg_size)

#### \--num-publishers, -P

Number of publishers, see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/pub/n](#scenarios/emqttb_scenario_persistent_session/{}/pub/n)

#### \--pubautorate

ID of the autorate config used to tune publish interval, see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/pub/pub\_autorate](#scenarios/emqttb_scenario_persistent_session/{}/pub/pub_autorate)

#### \--pubtime, -T

Period of time while publishing will last (ms), see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/pub/pub\_time](#scenarios/emqttb_scenario_persistent_session/{}/pub/pub_time)

#### \--pubinterval, -i

Message publishing interval (microsecond), see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/pub/pubinterval](#scenarios/emqttb_scenario_persistent_session/{}/pub/pubinterval)

#### \--pub-qos

QoS of the published messages, see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/pub/qos](#scenarios/emqttb_scenario_persistent_session/{}/pub/qos)

#### \--publatency

Try to keep publishing time at this value (ms), see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/pub/set\_pub\_latency](#scenarios/emqttb_scenario_persistent_session/{}/pub/set_pub_latency)

#### \--topic, -t

Suffix of the topic to publish to, see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/pub/topic\_suffix](#scenarios/emqttb_scenario_persistent_session/{}/pub/topic_suffix)

#### \--expiry

Session expiry interval, see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/sub/expiry](#scenarios/emqttb_scenario_persistent_session/{}/sub/expiry)

#### \--num-subscribers, -S

Number of subscribers, see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/sub/n](#scenarios/emqttb_scenario_persistent_session/{}/sub/n)

#### \--sub-qos

Subscription QoS, see:
[scenarios/emqttb\_scenario\_persistent\_session/{}/sub/qos](#scenarios/emqttb_scenario_persistent_session/{}/sub/qos)

## @pub

Run scenario pub

### Description

This scenario starts `-N` workers, which publish messages to the
specified topic at period `--pubinterval`. The only mandatory parameter
is `--topic`, which supports pattern substitutions.

Configuration of the publishers (such as broker hostname and port, tcp
settings, etc.) is delegated to [\#groups](#groups).

### Examples

#### Basic usage

``` bash
emqttb @pub -t foo/%n -N 100 -i 10ms -s 1kb
```

In this example the loadgen connects to the default broker
<mqtt://localhost:1883>, starts 100 publishers which send messages to
topic with the suffix of the worker id every 10 milliseconds. Size of
the messages is 1kb.

#### Changing client settings

``` bash
emqttb @pub -t foo/%n @g --ssl --transport ws -h 127.0.0.1
```

In this example settings of the default client group has been changed:
TLS encryption is enabled, and WebSocket transport is used. Also the
hostname of the broker is specified explicitly.

``` bash
emqttb @pub -t foo/%n -q 1 -g pub @g -g pub --ssl --transport ws -h 127.0.0.1
```

The below example is similar to the previous one, except QoS of the
messages is set to 1, and a dedicated client configuration with id `pub`
is used for the publishers. It's useful for running multiple scenarios
(e.g. `@pub` and `@sub`) in parallel, when they must use different
settings. For example, it can be used for testing MQTT bridge.

#### Tuning publishing rate automatically

By default, `@pub` scenario keeps `pubinterval` constant. However, in
some situations it should be tuned dynamically: suppose one wants to
measure what publishing rate the broker can sustain while keeping
publish latency under `--publatency`.

This is also useful for preventing system overload. Generating too much
load can bring the system down, and the test has to be started all over
again with different parameters. Sometimes finding the correct rate
takes many attempts, wasting human and machine time. Dynamic tuning of
the publishing rate for keeping the latency constant can help in this
situation.

By default the maximum speed of rate adjustment is set to 0, effectively
locking the `pubinterval` at a constant value. To enable automatic
tuning, the autorate speed `-V` must be set to a non-zero value, also it
makes sense to set the minimum (`-m`) and maximum (`-M`) values of the
`pubinterval`:

``` bash
emqttb @pub -t foo -i 1s -q 1 --publatency 50ms @a -V 10 -m 0 -M 10000
```

Once automatic adjustment of the publishing interval is enabled, `-i`
parameter sets the starting value of the publish interval, rather than
the constant value. So the above example reads like this:

Publish messages to topic `foo` with QoS 1, starting at the publishing
interval of 1000 milliseconds, dynamically adjusting it so to keep the
publishing latency around 50 milliseconds. The publishing interval is
kept between 0 and 10 seconds, and the maximum rate of its change is 10
milliseconds per second.

For more information about the automatic parameter tuning see
[\#autorate](#autorate).

### CLI arguments

#### \--conninterval, -I

Client connection interval (microsecond), see:
[scenarios/emqttb\_scenario\_pub/{}/conninterval](#scenarios/emqttb_scenario_pub/{}/conninterval)

#### \--group, -g

ID of the client group, see:
[scenarios/emqttb\_scenario\_pub/{}/group](#scenarios/emqttb_scenario_pub/{}/group)

#### \--loiter

Keep running scenario stages for this period of time (sec), see:
[scenarios/emqttb\_scenario\_pub/{}/loiter](#scenarios/emqttb_scenario_pub/{}/loiter)

#### \--metadata

Add metadata to the messages, see:
[scenarios/emqttb\_scenario\_pub/{}/metadata](#scenarios/emqttb_scenario_pub/{}/metadata)

#### \--size, -s

Size of the published message in bytes, see:
[scenarios/emqttb\_scenario\_pub/{}/msg\_size](#scenarios/emqttb_scenario_pub/{}/msg_size)

#### \--num-clients, -N

Number of clients, see:
[scenarios/emqttb\_scenario\_pub/{}/n\_clients](#scenarios/emqttb_scenario_pub/{}/n_clients)

#### \--pubautorate

ID of the autorate config used to tune publish interval, see:
[scenarios/emqttb\_scenario\_pub/{}/pub\_autorate](#scenarios/emqttb_scenario_pub/{}/pub_autorate)

#### \--pubinterval, -i

Message publishing interval (microsecond), see:
[scenarios/emqttb\_scenario\_pub/{}/pubinterval](#scenarios/emqttb_scenario_pub/{}/pubinterval)

#### \--qos, -q

QoS of the published messages, see:
[scenarios/emqttb\_scenario\_pub/{}/qos](#scenarios/emqttb_scenario_pub/{}/qos)

#### \--publatency

Try to keep publishing time at this value (ms), see:
[scenarios/emqttb\_scenario\_pub/{}/set\_pub\_latency](#scenarios/emqttb_scenario_pub/{}/set_pub_latency)

#### \--topic, -t

Topic where the clients shall publish messages, see:
[scenarios/emqttb\_scenario\_pub/{}/topic](#scenarios/emqttb_scenario_pub/{}/topic)

## @pubsub\_fwd

Run scenario pubsub\_fwd

### Description

First all subscribers connect and subscribe to the brokers, then the
publishers start to connect and publish. The default is to use full
forwarding of messages between the nodes: that is, each publisher client
publishes to a topic subscribed by a single client, and both clients
reside on distinct nodes.

Full forwarding of messages is the default and can be set by
[\#full\_forwarding](#full_forwarding).

### Examples

#### Basic usage

``` bash
./emqttb --restapi @pubsub_fwd --publatency 10ms --num-clients 400 -i 70ms @g -h 172.25.0.2:1883,172.25.0.3:1883,172.25.0.4:1883
```

In this example the loadgen connects to a list of brokers in a
round-robin in the declared order. First all the subscribers, then the
publishers, with the difference that publishers will shift the given
host list by one position to ensure each publisher and subscriber pair
will reside on different hosts, thus forcing all messages to be
forwarded.

### CLI arguments

#### \--conninterval, -I

Client connection interval (microsecond), see:
[scenarios/emqttb\_scenario\_pubsub\_forward/{}/conninterval](#scenarios/emqttb_scenario_pubsub_forward/{}/conninterval)

#### \--full-forwarding

Whether all messages should be forwarded between nodes, see:
[scenarios/emqttb\_scenario\_pubsub\_forward/{}/full\_forwarding](#scenarios/emqttb_scenario_pubsub_forward/{}/full_forwarding)

#### \--group, -g

ID of the client group, see:
[scenarios/emqttb\_scenario\_pubsub\_forward/{}/group](#scenarios/emqttb_scenario_pubsub_forward/{}/group)

#### \--loiter

Keep running scenario stages for this period of time (sec), see:
[scenarios/emqttb\_scenario\_pubsub\_forward/{}/loiter](#scenarios/emqttb_scenario_pubsub_forward/{}/loiter)

#### \--num-clients, -n

Total number of connections (pub + sub), see:
[scenarios/emqttb\_scenario\_pubsub\_forward/{}/num\_clients](#scenarios/emqttb_scenario_pubsub_forward/{}/num_clients)

#### \--size, -s

Size of the published message in bytes, see:
[scenarios/emqttb\_scenario\_pubsub\_forward/{}/pub/msg\_size](#scenarios/emqttb_scenario_pubsub_forward/{}/pub/msg_size)

#### \--pubautorate

ID of the autorate config used to tune publish interval, see:
[scenarios/emqttb\_scenario\_pubsub\_forward/{}/pub/pub\_autorate](#scenarios/emqttb_scenario_pubsub_forward/{}/pub/pub_autorate)

#### \--pubinterval, -i

Message publishing interval (microsecond), see:
[scenarios/emqttb\_scenario\_pubsub\_forward/{}/pub/pubinterval](#scenarios/emqttb_scenario_pubsub_forward/{}/pub/pubinterval)

#### \--pub-qos

QoS of the published messages, see:
[scenarios/emqttb\_scenario\_pubsub\_forward/{}/pub/qos](#scenarios/emqttb_scenario_pubsub_forward/{}/pub/qos)

#### \--publatency

Try to keep publishing time at this value (ms), see:
[scenarios/emqttb\_scenario\_pubsub\_forward/{}/pub/set\_pub\_latency](#scenarios/emqttb_scenario_pubsub_forward/{}/pub/set_pub_latency)

#### \--sub-qos

Subscription QoS, see:
[scenarios/emqttb\_scenario\_pubsub\_forward/{}/sub/qos](#scenarios/emqttb_scenario_pubsub_forward/{}/sub/qos)

## @sub

Run scenario sub

### CLI arguments

#### \--conninterval, -I

Client connection interval, see:
[scenarios/emqttb\_scenario\_sub/{}/conninterval](#scenarios/emqttb_scenario_sub/{}/conninterval)

#### \--expiry, -x

Set 'Session-Expiry' for persistent sessions (seconds), see:
[scenarios/emqttb\_scenario\_sub/{}/expiry](#scenarios/emqttb_scenario_sub/{}/expiry)

#### \--group, -g

ID of the client group, see:
[scenarios/emqttb\_scenario\_sub/{}/group](#scenarios/emqttb_scenario_sub/{}/group)

#### \--loiter

Keep running scenario stages for this period of time (sec), see:
[scenarios/emqttb\_scenario\_sub/{}/loiter](#scenarios/emqttb_scenario_sub/{}/loiter)

#### \--num-clients, -N

Number of clients, see:
[scenarios/emqttb\_scenario\_sub/{}/n\_clients](#scenarios/emqttb_scenario_sub/{}/n_clients)

#### \--qos, -q

QoS of the subscription, see:
[scenarios/emqttb\_scenario\_sub/{}/qos](#scenarios/emqttb_scenario_sub/{}/qos)

#### \--topic, -t

Topic that the clients shall subscribe, see:
[scenarios/emqttb\_scenario\_sub/{}/topic](#scenarios/emqttb_scenario_sub/{}/topic)

# OS Environment Variables

The following OS environment variables are used to set configuration
values. Values of type string() are taken from OS environment variables
verbatim, other types are parsed as Erlang terms.

Priority: 0

## EMQTTB\_CONVENIENCE\_\_CONF\_DUMP

Name of the repeat file or \`undefined\`, see:
[convenience/conf\_dump](#convenience/conf_dump)

## EMQTTB\_CONVENIENCE\_\_KEEP\_RUNNING

Keep the process running after completing all the scenarios, see:
[convenience/keep\_running](#convenience/keep_running)

## EMQTTB\_CONVENIENCE\_\_LOITER

Default loiter time for the scenarios (sec), see:
[convenience/loiter](#convenience/loiter)

## EMQTTB\_LOGGING\_\_DEFAULT\_HANDLER\_LEVEL

Log level for the default handler, see:
[logging/default\_handler\_level](#logging/default_handler_level)

## EMQTTB\_LOGGING\_\_LEVEL

Global log level, see: [logging/level](#logging/level)

## EMQTTB\_METRICS\_\_PUSHGATEWAY\_\_ENABLED

Enable sending metrics to pushgateway, see:
[metrics/pushgateway/enabled](#metrics/pushgateway/enabled)

## EMQTTB\_METRICS\_\_PUSHGATEWAY\_\_INTERVAL

Push interval (ms), see:
[metrics/pushgateway/interval](#metrics/pushgateway/interval)

## EMQTTB\_METRICS\_\_PUSHGATEWAY\_\_URL

URL of pushgateway server, see:
[metrics/pushgateway/url](#metrics/pushgateway/url)

## EMQTTB\_RESTAPI\_\_ENABLED

Enable REST API, see: [restapi/enabled](#restapi/enabled)

## EMQTTB\_RESTAPI\_\_LISTEN\_PORT

REST API listening interface/port, see:
[restapi/listen\_port](#restapi/listen_port)

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

# Maps

This section lists all maps.

## autorate

Autorate configuration

*Key elements:*

1.  [id](#autorate/{}/id)

When the loadgen creates too much traffic, the system may get
overloaded. In this case, the test usually has to be restarted all over
again with different parameters. This can be very expensive in man-hours
and computing resources.

In order to prevent that, emqttb can tune some parameters (such as
message publishing interval) automatically using [PI
contoller](https://controlguru.com/integral-reset-windup-jacketing-logic-and-the-velocity-pi-form/)

## groups

Configuration for client groups

*Key elements:*

1.  [id](#groups/{}/id)

*Description:*

It is possible to override client configuration for the group.

## scenarios/emqttb\_scenario\_make\_docs

Run scenario make-docs

*Key elements:*

## scenarios/emqttb\_scenario\_persistent\_session

Run scenario persistent\_session

*Key elements:*

## scenarios/emqttb\_scenario\_pub

Run scenario pub

*Key elements:*

### Description

This scenario starts `-N` workers, which publish messages to the
specified topic at period `--pubinterval`. The only mandatory parameter
is `--topic`, which supports pattern substitutions.

Configuration of the publishers (such as broker hostname and port, tcp
settings, etc.) is delegated to [\#groups](#groups).

### Examples

#### Basic usage

``` bash
emqttb @pub -t foo/%n -N 100 -i 10ms -s 1kb
```

In this example the loadgen connects to the default broker
<mqtt://localhost:1883>, starts 100 publishers which send messages to
topic with the suffix of the worker id every 10 milliseconds. Size of
the messages is 1kb.

#### Changing client settings

``` bash
emqttb @pub -t foo/%n @g --ssl --transport ws -h 127.0.0.1
```

In this example settings of the default client group has been changed:
TLS encryption is enabled, and WebSocket transport is used. Also the
hostname of the broker is specified explicitly.

``` bash
emqttb @pub -t foo/%n -q 1 -g pub @g -g pub --ssl --transport ws -h 127.0.0.1
```

The below example is similar to the previous one, except QoS of the
messages is set to 1, and a dedicated client configuration with id `pub`
is used for the publishers. It's useful for running multiple scenarios
(e.g. `@pub` and `@sub`) in parallel, when they must use different
settings. For example, it can be used for testing MQTT bridge.

#### Tuning publishing rate automatically

By default, `@pub` scenario keeps `pubinterval` constant. However, in
some situations it should be tuned dynamically: suppose one wants to
measure what publishing rate the broker can sustain while keeping
publish latency under `--publatency`.

This is also useful for preventing system overload. Generating too much
load can bring the system down, and the test has to be started all over
again with different parameters. Sometimes finding the correct rate
takes many attempts, wasting human and machine time. Dynamic tuning of
the publishing rate for keeping the latency constant can help in this
situation.

By default the maximum speed of rate adjustment is set to 0, effectively
locking the `pubinterval` at a constant value. To enable automatic
tuning, the autorate speed `-V` must be set to a non-zero value, also it
makes sense to set the minimum (`-m`) and maximum (`-M`) values of the
`pubinterval`:

``` bash
emqttb @pub -t foo -i 1s -q 1 --publatency 50ms @a -V 10 -m 0 -M 10000
```

Once automatic adjustment of the publishing interval is enabled, `-i`
parameter sets the starting value of the publish interval, rather than
the constant value. So the above example reads like this:

Publish messages to topic `foo` with QoS 1, starting at the publishing
interval of 1000 milliseconds, dynamically adjusting it so to keep the
publishing latency around 50 milliseconds. The publishing interval is
kept between 0 and 10 seconds, and the maximum rate of its change is 10
milliseconds per second.

For more information about the automatic parameter tuning see
[\#autorate](#autorate).

## scenarios/emqttb\_scenario\_pubsub\_forward

Run scenario pubsub\_fwd

*Key elements:*

### Description

First all subscribers connect and subscribe to the brokers, then the
publishers start to connect and publish. The default is to use full
forwarding of messages between the nodes: that is, each publisher client
publishes to a topic subscribed by a single client, and both clients
reside on distinct nodes.

Full forwarding of messages is the default and can be set by
[\#full\_forwarding](#full_forwarding).

### Examples

#### Basic usage

``` bash
./emqttb --restapi @pubsub_fwd --publatency 10ms --num-clients 400 -i 70ms @g -h 172.25.0.2:1883,172.25.0.3:1883,172.25.0.4:1883
```

In this example the loadgen connects to a list of brokers in a
round-robin in the declared order. First all the subscribers, then the
publishers, with the difference that publishers will shift the given
host list by one position to ensure each publisher and subscriber pair
will reside on different hosts, thus forcing all messages to be
forwarded.

## scenarios/emqttb\_scenario\_sub

Run scenario sub

*Key elements:*

# Values

This section lists all configurable values.

## autorate/{}/id

ID of the autorate configuration

*Type:*

``` erlang
atom()
```

*Default value:*

``` erlang
default
```

Autorate configuration can be referred by id.

## autorate/{}/k\_p

Controller gain

*Type:*

``` erlang
number()
```

*Default value:*

``` erlang
0.05
```

## autorate/{}/max

Maximum value of the controlled parameter

*Type:*

``` erlang
integer()
```

*Default value:*

``` erlang
100000000
```

## autorate/{}/min

Minimum value of the controlled parameter

*Type:*

``` erlang
integer()
```

*Default value:*

``` erlang
0
```

## autorate/{}/speed

Maximum rate of change of the controlled parameter

*Type:*

``` erlang
integer()
```

*Default value:*

``` erlang
0
```

## autorate/{}/t\_i

Controller reset time

*Type:*

``` erlang
number()
```

*Default value:*

``` erlang
1
```

## convenience/again

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

## convenience/conf\_dump

Name of the repeat file or \`undefined\`

*Type:*

``` erlang
string() | undefined when
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

## convenience/keep\_running

Keep the process running after completing all the scenarios

*Type:*

``` erlang
boolean()
```

*Default value:*

See [restapi/enabled](#restapi/enabled)

*Description:*

By default, when started without REST API, emqttb script terminates
after completing all the scenarios, which is useful for scripting.
However, when running with REST API, such behavior is undesirable. So
when REST is enabled, the default behavior is different: the process
keeps running waiting for commands.

This flag can be used to explicitly override this behavior.

## convenience/loiter

Default loiter time for the scenarios (sec)

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

## groups/{}/autoscale

Pointer at autorate configuration

*Type:*

``` erlang
atom()
```

*Default value:*

``` erlang
default
```

## groups/{}/broker/hosts

Hostname of the target broker

*Type:*

``` erlang
emqttb:hosts() when
  emqttb:hosts() :: [string() | {string(), emqttb:net_port()}],
  emqttb:net_port() :: 1..65535,
  string() :: [char()].
```

*Default value:*

``` erlang
["localhost"]
```

## groups/{}/broker/port

Port of the target broker

*Type:*

``` erlang
emqttb:net_port() | default when
  emqttb:net_port() :: 1..65535.
```

*Default value:*

``` erlang
default
```

## groups/{}/client/clientid

Clientid pattern

*Type:*

``` erlang
binary()
```

*Default value:*

``` erlang
<<"%h-%g-%n">>
```

A pattern used to generate ClientID. The following substitutions are
supported:

  - `%n` is replaced with the worker ID (integer)

  - `%g` is replaced with the group ID

  - `%h` is replaced with the hostname

## groups/{}/client/password

Password for connecting to the broker

*Type:*

``` erlang
undefined | string() when
  string() :: [char()].
```

*Default value:*

``` erlang
undefined
```

## groups/{}/client/username

Username of the client

*Type:*

``` erlang
undefined | string() when
  string() :: [char()].
```

*Default value:*

``` erlang
undefined
```

## groups/{}/connection/inflight

maximum inflight messages for QoS 1 and 2

*Type:*

``` erlang
non_neg_integer() | infinity when
  non_neg_integer() :: 0..inf.
```

*Default value:*

``` erlang
10
```

## groups/{}/connection/proto\_ver

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

## groups/{}/connection/transport

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

## groups/{}/id

ID of the group

*Type:*

``` erlang
atom()
```

*Default value:*

``` erlang
default
```

## groups/{}/lowmem

Reduce memory useage at the cost of CPU wherever possible

*Type:*

``` erlang
boolean()
```

*Default value:*

``` erlang
false
```

## groups/{}/net/ifaddr

Local IP addresses

*Type:*

``` erlang
emqttb:ifaddr_list() when
  emqttb:ifaddr_list() :: [ip_address(),...],
  ip4_address() :: {0..255, 0..255, 0..255, 0..255},
  ip6_address() :: {0..65535, 0..65535, 0..65535, 0..65535, 0..65535, 0..65535, 0..65535, 0..65535},
  ip_address() :: ip6_address() | ip4_address().
```

*Default value:*

``` erlang
[{0,0,0,0}]
```

## groups/{}/ssl/certfile

Client certificate for authentication, if required by the server

*Type:*

``` erlang
string() when
  string() :: [char()].
```

*Default value:*

``` erlang
[]
```

## groups/{}/ssl/enable

Enable SSL for the connections

*Type:*

``` erlang
boolean()
```

*Default value:*

``` erlang
false
```

## groups/{}/ssl/keyfile

Client private key for authentication, if required by the server

*Type:*

``` erlang
string() when
  string() :: [char()].
```

*Default value:*

``` erlang
[]
```

## interval

Default interval between events

*Type:*

``` erlang
emqttb:duration_us() when
  emqttb:duration_us() :: integer().
```

*Default value:*

``` erlang
100000
```

It is possible to specify time unit as *ms*, *us* or *s*.

## logging/default\_handler\_level

Log level for the default handler

*Type:*

``` erlang
lee_logger:level() when
  lee_logger:level() :: alert | critical | error | warning | notice | info | debug.
```

*Default value:*

See [logging/level](#logging/level)

## logging/level

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

## metrics/pushgateway/enabled

Enable sending metrics to pushgateway

*Type:*

``` erlang
boolean()
```

*Default value:*

``` erlang
false
```

## metrics/pushgateway/interval

Push interval (ms)

*Type:*

``` erlang
emqttb:duration_ms() when
  emqttb:duration_ms() :: integer().
```

*Default value:*

``` erlang
1000
```

## metrics/pushgateway/url

URL of pushgateway server

*Type:*

``` erlang
string() when
  string() :: [char()].
```

*Default value:*

``` erlang
"http://localhost:9091"
```

## n\_clients

Maximum number of clients used by default by all groups

*Type:*

``` erlang
0..16777116
```

*Default value:*

``` erlang
1000
```

## restapi/enabled

Enable REST API

*Type:*

``` erlang
boolean()
```

*Default value:*

``` erlang
false
```

*Description:*

### REST API endpoints

By default, REST API is disabled and emqttb runs in script mode. To
enable it, run the script with --restapi flag.

#### Methods

[/healthcheck](http://localhost:8017/healthcheck): Healthcheck endpoint.
Just returns 200 all the time.

[/metrics](http://localhost:8017/metrics): Prometheus metrics endpoiont

[/scenario/:scenario/stage](http://localhost:8017/scenario/:scenario/stage):
Returns the currently running stage of a scenario.

[/conf/reload](http://localhost:8017/conf/reload): Reload configuration
in the runtime.

## restapi/listen\_port

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

## scenarios/emqttb\_scenario\_make\_docs/{}/external\_doc

Path to the external documentation source file

*Type:*

``` erlang
string() when
  string() :: [char()].
```

## scenarios/emqttb\_scenario\_make\_docs/{}/loiter

Keep running scenario stages for this period of time (sec)

*Type:*

``` erlang
timeout() when
  non_neg_integer() :: 0..inf,
  timeout() :: non_neg_integer() | infinity.
```

*Default value:*

See [convenience/loiter](#convenience/loiter)

## scenarios/emqttb\_scenario\_persistent\_session/{}/conninterval

Client connection interval (microsecond)

*Type:*

``` erlang
emqttb:duration_us() when
  emqttb:duration_us() :: integer().
```

*Default value:*

``` erlang
0
```

## scenarios/emqttb\_scenario\_persistent\_session/{}/group

ID of the client group

*Type:*

``` erlang
atom()
```

*Default value:*

``` erlang
default
```

## scenarios/emqttb\_scenario\_persistent\_session/{}/loiter

Keep running scenario stages for this period of time (sec)

*Type:*

``` erlang
timeout() when
  non_neg_integer() :: 0..inf,
  timeout() :: non_neg_integer() | infinity.
```

*Default value:*

See [convenience/loiter](#convenience/loiter)

## scenarios/emqttb\_scenario\_persistent\_session/{}/n\_cycles

How many times to repeat publish/consume cycle

*Type:*

``` erlang
non_neg_integer() | inifinity when
  non_neg_integer() :: 0..inf.
```

*Default value:*

``` erlang
10
```

## scenarios/emqttb\_scenario\_persistent\_session/{}/pub/msg\_size

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

## scenarios/emqttb\_scenario\_persistent\_session/{}/pub/n

Number of publishers

*Type:*

``` erlang
0..16777116
```

*Default value:*

``` erlang
10
```

## scenarios/emqttb\_scenario\_persistent\_session/{}/pub/pub\_autorate

ID of the autorate config used to tune publish interval

*Type:*

``` erlang
atom()
```

*Default value:*

``` erlang
default
```

## scenarios/emqttb\_scenario\_persistent\_session/{}/pub/pub\_time

Period of time while publishing will last (ms)

*Type:*

``` erlang
emqttb:duration_ms() when
  emqttb:duration_ms() :: integer().
```

*Default value:*

``` erlang
1000
```

## scenarios/emqttb\_scenario\_persistent\_session/{}/pub/pubinterval

Message publishing interval (microsecond)

*Type:*

``` erlang
emqttb:duration_us() when
  emqttb:duration_us() :: integer().
```

*Default value:*

See [interval](#interval)

## scenarios/emqttb\_scenario\_persistent\_session/{}/pub/qos

QoS of the published messages

*Type:*

``` erlang
emqttb:qos() when
  emqttb:qos() :: 0..2.
```

*Default value:*

``` erlang
2
```

Warning: changing QoS to any value other then 2 is likely to cause
consume stage to hang, since it has to consume the exact number of
messages as previously produced.

## scenarios/emqttb\_scenario\_persistent\_session/{}/pub/set\_pub\_latency

Try to keep publishing time at this value (ms)

*Type:*

``` erlang
emqttb:duration_ms() when
  emqttb:duration_ms() :: integer().
```

*Default value:*

``` erlang
100
```

## scenarios/emqttb\_scenario\_persistent\_session/{}/pub/topic\_suffix

Suffix of the topic to publish to

*Type:*

``` erlang
binary()
```

*Default value:*

``` erlang
<<"%h/%n">>
```

## scenarios/emqttb\_scenario\_persistent\_session/{}/sub/expiry

Session expiry interval

*Type:*

``` erlang
non_neg_integer() when
  non_neg_integer() :: 0..inf.
```

*Default value:*

``` erlang
4294967295
```

## scenarios/emqttb\_scenario\_persistent\_session/{}/sub/n

Number of subscribers

*Type:*

``` erlang
0..16777116
```

*Default value:*

``` erlang
10
```

## scenarios/emqttb\_scenario\_persistent\_session/{}/sub/qos

Subscription QoS

*Type:*

``` erlang
emqttb:qos() when
  emqttb:qos() :: 0..2.
```

*Default value:*

``` erlang
2
```

Warning: changing QoS to any value other then 2 is likely to cause
consume stage to hang, since it has to consume the exact number of
messages as previously produced.

## scenarios/emqttb\_scenario\_pub/{}/conninterval

Client connection interval (microsecond)

*Type:*

``` erlang
emqttb:duration_us() when
  emqttb:duration_us() :: integer().
```

*Default value:*

See [interval](#interval)

## scenarios/emqttb\_scenario\_pub/{}/group

ID of the client group

*Type:*

``` erlang
atom()
```

*Default value:*

``` erlang
default
```

## scenarios/emqttb\_scenario\_pub/{}/loiter

Keep running scenario stages for this period of time (sec)

*Type:*

``` erlang
timeout() when
  non_neg_integer() :: 0..inf,
  timeout() :: non_neg_integer() | infinity.
```

*Default value:*

See [convenience/loiter](#convenience/loiter)

## scenarios/emqttb\_scenario\_pub/{}/metadata

Add metadata to the messages

*Type:*

``` erlang
boolean()
```

*Default value:*

``` erlang
false
```

## scenarios/emqttb\_scenario\_pub/{}/msg\_size

Size of the published message in bytes

*Type:*

``` erlang
emqttb:byte_size() when
  emqttb:byte_size() :: non_neg_integer(),
  non_neg_integer() :: 0..inf.
```

*Default value:*

``` erlang
256
```

## scenarios/emqttb\_scenario\_pub/{}/n\_clients

Number of clients

*Type:*

``` erlang
0..16777116
```

*Default value:*

See [n\_clients](#n_clients)

## scenarios/emqttb\_scenario\_pub/{}/pub\_autorate

ID of the autorate config used to tune publish interval

*Type:*

``` erlang
atom()
```

*Default value:*

``` erlang
default
```

## scenarios/emqttb\_scenario\_pub/{}/pubinterval

Message publishing interval (microsecond)

*Type:*

``` erlang
emqttb:duration_us() when
  emqttb:duration_us() :: integer().
```

*Default value:*

See [interval](#interval)

## scenarios/emqttb\_scenario\_pub/{}/qos

QoS of the published messages

*Type:*

``` erlang
emqttb:qos() when
  emqttb:qos() :: 0..2.
```

*Default value:*

``` erlang
0
```

## scenarios/emqttb\_scenario\_pub/{}/set\_pub\_latency

Try to keep publishing time at this value (ms)

*Type:*

``` erlang
emqttb:duration_ms() when
  emqttb:duration_ms() :: integer().
```

*Default value:*

``` erlang
100
```

## scenarios/emqttb\_scenario\_pub/{}/topic

Topic where the clients shall publish messages

*Type:*

``` erlang
binary()
```

Topic is a mandatory parameter. It supports the following substitutions:

  - `%n` is replaced with the worker ID (integer)

  - `%g` is replaced with the group ID

  - `%h` is replaced with the hostname

## scenarios/emqttb\_scenario\_pubsub\_forward/{}/conninterval

Client connection interval (microsecond)

*Type:*

``` erlang
emqttb:duration_us() when
  emqttb:duration_us() :: integer().
```

*Default value:*

``` erlang
0
```

## scenarios/emqttb\_scenario\_pubsub\_forward/{}/full\_forwarding

Whether all messages should be forwarded between nodes

*Type:*

``` erlang
boolean()
```

*Default value:*

``` erlang
true
```

## scenarios/emqttb\_scenario\_pubsub\_forward/{}/group

ID of the client group

*Type:*

``` erlang
atom()
```

*Default value:*

``` erlang
default
```

## scenarios/emqttb\_scenario\_pubsub\_forward/{}/loiter

Keep running scenario stages for this period of time (sec)

*Type:*

``` erlang
timeout() when
  non_neg_integer() :: 0..inf,
  timeout() :: non_neg_integer() | infinity.
```

*Default value:*

See [convenience/loiter](#convenience/loiter)

## scenarios/emqttb\_scenario\_pubsub\_forward/{}/num\_clients

Total number of connections (pub + sub)

*Type:*

``` erlang
0..16777116
```

*Default value:*

``` erlang
100
```

## scenarios/emqttb\_scenario\_pubsub\_forward/{}/pub/msg\_size

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

## scenarios/emqttb\_scenario\_pubsub\_forward/{}/pub/pub\_autorate

ID of the autorate config used to tune publish interval

*Type:*

``` erlang
atom()
```

*Default value:*

``` erlang
default
```

## scenarios/emqttb\_scenario\_pubsub\_forward/{}/pub/pubinterval

Message publishing interval (microsecond)

*Type:*

``` erlang
emqttb:duration_us() when
  emqttb:duration_us() :: integer().
```

*Default value:*

See [interval](#interval)

## scenarios/emqttb\_scenario\_pubsub\_forward/{}/pub/qos

QoS of the published messages

*Type:*

``` erlang
emqttb:qos() when
  emqttb:qos() :: 0..2.
```

*Default value:*

``` erlang
1
```

## scenarios/emqttb\_scenario\_pubsub\_forward/{}/pub/set\_pub\_latency

Try to keep publishing time at this value (ms)

*Type:*

``` erlang
emqttb:duration_ms() when
  emqttb:duration_ms() :: integer().
```

*Default value:*

``` erlang
100
```

## scenarios/emqttb\_scenario\_pubsub\_forward/{}/sub/qos

Subscription QoS

*Type:*

``` erlang
emqttb:qos() when
  emqttb:qos() :: 0..2.
```

*Default value:*

``` erlang
1
```

## scenarios/emqttb\_scenario\_sub/{}/conninterval

Client connection interval

*Type:*

``` erlang
emqttb:duration_us() when
  emqttb:duration_us() :: integer().
```

*Default value:*

See [interval](#interval)

## scenarios/emqttb\_scenario\_sub/{}/expiry

Set 'Session-Expiry' for persistent sessions (seconds)

*Type:*

``` erlang
non_neg_integer() | undefined when
  non_neg_integer() :: 0..inf.
```

*Default value:*

``` erlang
undefined
```

## scenarios/emqttb\_scenario\_sub/{}/group

ID of the client group

*Type:*

``` erlang
atom()
```

*Default value:*

``` erlang
default
```

## scenarios/emqttb\_scenario\_sub/{}/loiter

Keep running scenario stages for this period of time (sec)

*Type:*

``` erlang
timeout() when
  non_neg_integer() :: 0..inf,
  timeout() :: non_neg_integer() | infinity.
```

*Default value:*

See [convenience/loiter](#convenience/loiter)

## scenarios/emqttb\_scenario\_sub/{}/n\_clients

Number of clients

*Type:*

``` erlang
0..16777116
```

*Default value:*

See [n\_clients](#n_clients)

## scenarios/emqttb\_scenario\_sub/{}/qos

QoS of the subscription

*Type:*

``` erlang
emqttb:qos() when
  emqttb:qos() :: 0..2.
```

*Default value:*

``` erlang
0
```

## scenarios/emqttb\_scenario\_sub/{}/topic

Topic that the clients shall subscribe

*Type:*

``` erlang
binary()
```
