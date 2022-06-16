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
-module(emqttb_metrics).

-behavior(gen_server).

%% API:
-export([new_counter/2, counter_inc/2, counter_dec/2, get_counter/1,
         new_gauge/2, gauge_observe/2, get_gauge/1
        ]).

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% prometheus callbacks:
% -export([deregister_cleanup/1, collect_mf/2, collect_metrics/2]).

%% internal exports:
-export([start_link/0]).

-export_type([]).

-compile({inline, [counter_inc/2, counter_dec/2]}).

%%================================================================================
%% Type declarations
%%================================================================================

-type key() :: {atom(), atom() | list() | tuple()}.

-define(SERVER, ?MODULE).

%% Persistent term keys:
-define(C(KEY), {emqttb_metrics_counter, KEY}).
-define(G(KEY), {emqttb_metrics_gauge, KEY}).

-define(TICK_TIME, 100).
-define(MAX_WINDOW_SEC, 30). % Keep about 30 seconds of history
-define(DEFAULT_WINDOW, 5000).

%%================================================================================
%% API funcions
%%================================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Simple counters:

-spec new_counter(key(), list()) -> key().
new_counter(Key, PrometheusParams) ->
  ok = gen_server:call(?SERVER, {new_counter, Key, PrometheusParams}),
  Key.

-spec counter_inc(key(), integer()) -> ok.
counter_inc(Key, Delta) ->
  counters:add(persistent_term:get(?C(Key)), 1, Delta).

-spec counter_dec(key(), integer()) -> ok.
counter_dec(Key, Delta) ->
  counters:sub(persistent_term:get(?C(Key)), 1, Delta).

-spec get_counter(key()) -> integer().
get_counter(Key) ->
  counters:get(persistent_term:get(?C(Key)), 1).

%% Rolling average "gauge":

-spec new_gauge(key(), list()) -> key().
new_gauge(Key, PrometheusParams) ->
  ok = gen_server:call(?SERVER, {new_gauge, Key, PrometheusParams}),
  Key.

%% Fast update of rolling average of a value:
-spec gauge_observe(key(), integer()) -> ok.
gauge_observe(Key, Val) ->
  Cnt = persistent_term:get(?G(Key)),
  counters:add(Cnt, 1, Val), %% Update the sum
  counters:add(Cnt, 2, 1).   %% Update the number of samples

-spec get_gauge(key()) -> integer().
get_gauge(Key) ->
  get_gauge(Key, ?DEFAULT_WINDOW).

-spec get_gauge(key(), non_neg_integer()) -> integer().
get_gauge(Key, Window) ->
  case gen_server:call(?SERVER, {get_gauge, Key, Window}) of
    undefined ->
      error({missing_gauge, Key});
    Val ->
      Val
  end.

%%================================================================================
%% gen_server callbacks
%%================================================================================

%% gen_server is needed to maintain the sliding windows and sync
%% metrics with prometheus.

%% Rolling average datapoint:
-record(p,
        { sum     :: integer()
        , samples :: integer()
        , time    :: integer()
        }).

%% Rolling average's state:
-record(r,
        { key        :: key()
        , datapoints :: [#p{}]
        }).

-record(s,
        { counters        = [] :: [key()]
        , rolling_average = [] :: [#r{}]
        }).

init([]) ->
  self() ! tick,
  {ok, #s{}}.

handle_call({new_counter, Key, PrometheusParams}, _From, S) ->
  {reply, ok, mk_counter(S, Key, PrometheusParams)};
handle_call({new_gauge, Key, PrometheusParams}, _From, S) ->
  {reply, ok, mk_rolling_average(S, Key, PrometheusParams)};
handle_call({get_gauge, Key, Window}, _From, S) ->
  Ret = case lists:keyfind(Key, 1, S#s.rolling_average) of
          false -> undefined;
          RA    -> do_get_gauge(RA, Window)
        end,
  {reply, ok, Ret};
handle_call(_Call, _From, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_Call, S) ->
  {noreply, S}.

handle_info(tick, S0) ->
  erlang:send_after(?TICK_TIME, self(), tick),
  S = collect_gauges(S0),
  update_prometheus(S),
  {noreply, S};
handle_info(_, S) ->
  {noreply, S}.

terminate(_Reason, S) ->
  {ok, S}.

%%================================================================================
%% Internal functions
%%================================================================================

collect_gauges(S = #s{rolling_average = RA}) ->
  S#s{rolling_average = lists:map(fun do_collect_gauge/1, RA)}.

do_get_gauge(#r{key = Key, datapoints = DP}, Window) ->
  #p{ sum = Sum
    , samples = N
    , time = T
    } = collect_datapoint(Key),
  #p{ sum = Sum0
    , samples = N0
    } = look_back(T - Window, DP),
  if N =:= N0 ->
      0;
     true ->
      (Sum - Sum0) div (N - N0)
  end.

look_back(StartTime, [A]) ->
  A;
look_back(StartTime, [A|Rest]) ->
  if A#p.time =< StartTime ->
      A;
     true ->
      look_back(StartTime, Rest)
  end.

do_collect_gauge(R0 = #r{key = K, datapoints = DP0}) ->
  DP = lists:droplast(DP0),
  R0#r{datapoints = [collect_datapoint(K) | DP]}.

collect_datapoint(K) ->
  Cnt = persistent_term:get(?G(K)),
  #p{ sum = counters:get(Cnt, 1)
    , samples = counters:get(Cnt, 2)
    , time = os:system_time(millisecond)
    }.

%% Really ugly stuff, like everything about prometheus...
update_prometheus(#s{counters = CC, rolling_average = RR}) ->
  lists:foreach(
    fun(K = {Tag, Label}) ->
        LabelList = prom_labels(Label),
        ok = prometheus_gauge:set(Tag, LabelList, get_counter(K))
    end,
    CC),
  lists:foreach(
    fun(R = #r{key = {Tag, Label}}) ->
        LabelList = prom_labels(Label),
        ok = prometheus_gauge:set(Tag, LabelList, do_get_gauge(R, ?DEFAULT_WINDOW))
    end,
    RR).

mk_rolling_average(S = #s{rolling_average = RR}, Key = {Tag, _Labels}, PrometheusParams) ->
  case lists:keyfind(Key, #r.key, RR) of
    false ->
      prometheus_gauge:declare([{name, Tag}|PrometheusParams]),
      Cnt = counters:new(2, [write_concurrency]),
      persistent_term:put(?G(Key), Cnt),
      DummyPoint = #p{ sum = 0
                     , samples = 0
                     , time = os:system_time(millisecond)
                     },
      NBuckets = timer:seconds(?MAX_WINDOW_SEC) div ?TICK_TIME,
      Datapoints = [DummyPoint || _ <- lists:seq(1, NBuckets)],
      R = #r{ key        = Key
            , datapoints = Datapoints
            },
      S#s{rolling_average = [R|RR]};
    _ ->
      S
  end.

mk_counter(S = #s{counters = CC}, Key = {Tag, _Labels}, PrometheusParams) ->
  case lists:member(Key, CC) of
    false ->
      prometheus_gauge:declare([{name, Tag}|PrometheusParams]),
      Cnt = counters:new(1, [write_concurrency]),
      persistent_term:put(?C(Key), Cnt),
      S#s{counters = [Key|CC]};
    true ->
      S
  end.

prom_labels(Label) ->
  if is_list(Label) ->
      Label;
     is_tuple(Label) ->
      tuple_to_list(Label);
     is_atom(Label); is_binary(Label) ->
      [Label]
  end.
