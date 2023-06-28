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
  ?CFG([cluster, enabled]) andalso
    start_distr(),
  ok.

start_distr() ->
  os:cmd("epmd -daemon"),
  Opts = #{dist_listen => true, name_domain => shortnames},
  Name = list_to_atom("emqttb-" ++ [$A + rand:uniform($Z-$A) - 1 || _ <- lists:seq(1, 5)]),
  net_kernel:start(Name, Opts),
  logger:notice("Started distribution with name: ~p", [node()]).
