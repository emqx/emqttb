%%%-------------------------------------------------------------------
%% @doc emqttb public API
%% @end
%%%-------------------------------------------------------------------

-module(emqttb_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("typerefl/include/types.hrl").
-include("emqttb.hrl").

start(_StartType, _StartArgs) ->
  declare_metrics(),
  Sup = emqttb_sup:start_link(),
  emqttb_conf:load_conf(application:get_env(emqttb, cli_opts, [])),
  post_init(),
  %emqttb_http:start(),
  Sup.

stop(_State) ->
  ok.

%% internal functions

declare_metrics() ->
  prometheus_gauge:declare([{name, bench_clients},
                            {help, <<"Number of clients in a group">>},
                            {labels, [group]}]).

%% Start misc. processes that depend on configuration
post_init() ->
  ?CFG([rest, enabled]) andalso
    emqttb_misc_sup:start_worker( emqttb_http
                                , {emqttb_http, start_link, []}
                                ),
  ok.
