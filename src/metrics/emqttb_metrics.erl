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

-type key() :: {atom(), atom() | list()}.

-define(SERVER, ?MODULE).

%% Persistent term keys:
-define(C(KEY), {emqttb_metrics_counter, KEY}).
-define(G(KEY), {emqttb_metrics_gauge, KEY}).

-define(TICK_TIME, 100).
-define(N_BUCKETS, 2).

%%================================================================================
%% API funcions
%%================================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Simple counters:

-spec new_counter(key(), list()) -> ok.
new_counter(Key, PrometheusParams) ->
  gen_server:call(?SERVER, {new_counter, Key, PrometheusParams}).

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

-spec new_gauge(key(), non_neg_integer()) -> ok.
new_gauge(Key, Window) ->
  gen_server:call(?SERVER, {new_gauge, Key, Window}).

%% Fast update of rolling average of a value:
-spec gauge_observe(key(), integer()) -> ok.
gauge_observe(Key, Val) ->
  Cnt = persistent_term:get(?G(Key)),
  counters:add(Cnt, 1, Val), %% Update the sum
  counters:add(Cnt, 2, 1).   %% Update the number of samples

-spec get_gauge(key()) -> integer().
get_gauge(Key) ->
  gen_server:call(?SERVER, {get_gauge, Key}).

%%================================================================================
%% gen_server callbacks
%%================================================================================

%% gen_server is needed to maintain the sliding windows and sync
%% metrics with prometheus.

%% Rolling average datapoint:
-record(p,
        { sum     :: integer()
        , samples :: integer()
        }).

%% Rolling average's state:
-record(r,
        { key        :: key()
        , window     :: non_neg_integer()
        , datapoints :: queue:queue(#p{})
        , time       :: integer()
        }).

-record(s,
        { simple_gauges   = [] :: [key()]
        , rolling_average = [] :: [#r{}]
        }).

init([]) ->
  self() ! tick,
  {ok, #s{}}.

handle_call({new_counter, Key, PrometheusParams}, _From, S) ->
  {reply, ok, mk_counter(S, Key, PrometheusParams)};
handle_call(_Call, _From, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_Call, S) ->
  {noreply, S}.

handle_info(tick, S) ->
  erlang:send_after(?TICK_TIME, self(), tick),
  update_prometheus(S),
  {noreply, S};
handle_info(_, S) ->
  {noreply, S}.

terminate(_Reason, S) ->
  {ok, S}.

%%================================================================================
%% Internal functions
%%================================================================================

%% Really ugly stuff, like everything about prometheus...
update_prometheus(#s{simple_gauges = G}) ->
  lists:foreach(
    fun(K = {Tag, Label}) ->
        LabelList = if is_list(Label) ->
                        Label;
                       is_tuple(Label) ->
                        tuple_to_list(Label);
                       is_atom(Label); is_binary(Label) ->
                        [Label]
                    end,
        ok = prometheus_gauge:set(Tag, LabelList, get_counter(K))
    end,
    G).

mk_counter(S = #s{simple_gauges = G}, Key = {Tag, _Labels}, PrometheusParams) ->
  prometheus_gauge:declare([{name, Tag}|PrometheusParams]),
  case lists:member(Key, G) of
    false ->
      Cnt = counters:new(1, [write_concurrency]),
      persistent_term:put(?C(Key), Cnt),
      S#s{simple_gauges = [Key|G]};
    true ->
      S
  end.

%% mk_gauge(S = #s{rolling_average = A}, Key, PTKey, Window) ->
%%   case lists:keyfind(Key, 1, A) of
%%     false ->
%%       Cnt = counters:new(2, [write_concurrency]),
%%       persistent_term:put(?G(Key), Cnt),
%%       DummyPoint = #p{sum = 0, samples = 0},
%%       Datapoints = queue:from_list([DummyPoint || _ <- lists:seq(1, ?N_BUCKETS)])
%%       R = #r{ window     = Window
%%             , datapoints = Datapoints
%%             , time       = os:system_time(millisecond)
%%             },
%%       S#s{rolling_average = [{Key, R}|A]};
%%     _ ->
%%       S
%%   end.
