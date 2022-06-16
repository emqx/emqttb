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
-module(emqtt_gauge_SUITE).


-compile(nowarn_export_all).
-compile(export_all).

-include_lib("snabbkaffe/include/snabbkaffe.hrl").
-include_lib("stdlib/include/assert.hrl").

suite() ->
  [{timetrap, {seconds, 30}}].

all() ->
  snabbkaffe:mk_all(?MODULE).

init_per_testcase(_, Config) ->
  snabbkaffe:fix_ct_logging(),
  {ok, _} = application:ensure_all_started(emqttb),
  Config.

end_per_testcase(_, _Config) ->
  application:stop(emqttb),
  snabbkaffe:stop(),
  ok.

t_gauge(Config) ->
  N = 10,
  G = emqttb_metrics:new_gauge({foo, []}, [{help, <<"">>}]),
  ?assertEqual(0, emqttb_metrics:get_gauge(G)),
  [emqttb_metrics:gauge_observe(G, 10) || _ <- lists:seq(1, N)],
  ?assertEqual(10, emqttb_metrics:get_gauge(G)),
  timer:sleep(1000),
  ?assertEqual(0, emqttb_metrics:get_gauge(G, 500)),
  %% Check value with a larger window:
  ?assertEqual(10, emqttb_metrics:get_gauge(G, 5000)),
  %% Add more samples:
  [emqttb_metrics:gauge_observe(G, 20) || _ <- lists:seq(1, N)],
  ?assertEqual(20, emqttb_metrics:get_gauge(G, 500)),
  %% Large window must produce the average of 20 and 10:
  ?assertEqual(15, emqttb_metrics:get_gauge(G, 5000)).
