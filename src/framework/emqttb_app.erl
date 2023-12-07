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
  emqttb_conf:load_model(),
  Sup = emqttb_sup:start_link(),
  emqttb_conf:load_conf(),
  maybe_perform_special_action(),
  emqttb_autorate:create_autorates(),
  emqttb_scenario:run_scenarios(),
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
      Opts = #{dist_listen => true},
      net_kernel:start(Name, Opts)
  end.

maybe_perform_special_action() ->
  case ?CFG_LIST([actions, ls, {}]) of
    [] ->
      ok;
    [Key] ->
      What = ?CFG(Key ++ [what]),
      Keys = lee_model:get_metatype_index(What, ?MYMODEL),
      MP = case What of
             metric -> id;
             autorate -> autorate_id
           end,
      lists:foreach(
        fun(K) ->
            #mnode{metaparams = #{MP := Id}} = lee_model:get(K, ?MYMODEL),
            io:format("~p~n", [Id])
        end,
        Keys),
      emqttb:terminate()
  end.
