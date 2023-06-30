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
  Sup = emqttb_sup:start_link(),
  emqttb_conf:load_conf(),
  CLIArgs = application:get_env(?APP, cli_args, []),
  emqttb_grafana:annotate(["Start emqttb " | lists:join($ , CLIArgs)]),
  post_init(),
  Sup.

stop(_State) ->
  emqttb_grafana:annotate("Stop emqttb"),
  ok.

%% internal functions

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
  emqttb_logger:setup(),
  maybe_start_distr(),
  ok.

maybe_start_distr() ->
  case ?CFG([cluster, node_name]) of
    undefined ->
      ok;
    Name ->
      os:cmd("epmd -daemon"),
      Opts = #{dist_listen => true, name_domain => shortnames},
      net_kernel:start(Name, Opts)
  end.
