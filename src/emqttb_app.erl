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
  emqttb_conf:load_conf(),
  ?CFG([convenience, keep_running]) orelse
    emqttb_scenario_sup:enable_autostop(),
  post_init(),
  Sup.

stop(_State) ->
  ok.

%% internal functions

declare_metrics() ->
  prometheus_gauge:declare([{name, num_clients},
                            {help, <<"Number of connected clients in the group">>},
                            {labels, [group]}]).

%% Start misc. processes that depend on configuration
post_init() ->
  ?CFG([restapi, enabled]) andalso
    emqttb_misc_sup:start_worker( emqttb_http
                                , {emqttb_http, start_link, []}
                                ),
  ?CFG([metrics, pushgateway, enabled]) andalso
    emqttb_misc_sup:start_worker( emqttb_pushgw
                                , {emqttb_pushgw, start_link, []}
                                ),
  ok.
