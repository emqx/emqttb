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
  maybe_increase_fd_limit(),
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
  ok.

maybe_increase_fd_limit() ->
  case os:type() of
    {unix, linux} ->
      %% Best effort to increase the soft limit
      Port = open_port({spawn, "prlimit --nofile=$(ulimit -Hn): --pid " ++ os:getpid()}, [exit_status, nouse_stdio]),
      Success =
        receive
          {Port, {exit_status, Status}} ->
            Status =:= 0
        after
          1000 ->
            false
        end,
      Success orelse
        logger:warning("Couldn't update soft FD limit"),
      ok;
    _ ->
      ok
  end.
