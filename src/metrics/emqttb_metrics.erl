%%--------------------------------------------------------------------
%% Copyright (c) 2022-2023 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-behavior(lee_metatype).

%% API:
-export([from_model/1, opstat_from_model/1, opstat/2, call_with_counter/4,
         new_counter/2, counter_inc/2, counter_dec/2, get_counter/1,
         new_gauge/2, gauge_set/2, gauge_ref/1,
         new_rolling_average/2, rolling_average_observe/2,
         get_rolling_average/1, get_rolling_average/2
        ]).

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% lee_metatype callbacks:
-export([names/1, metaparams/1, meta_validate/2]).

%% internal exports:
-export([start_link/0]).

-export_type([metric_ref/0]).

-compile({inline, [counter_inc/2, counter_dec/2]}).

-include("emqttb.hrl").
-include("../framework/emqttb_internal.hrl").
-include_lib("typerefl/include/types.hrl").
-include_lib("lee/include/lee.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type metric_id() :: {atom(), atom() | list() | tuple()}.

-reflect_type([metric_id/0]).

-define(SERVER, ?MODULE).

%% Persistent term keys:
-define(C(KEY), {emqttb_metrics_counter, KEY}).
-define(RA(KEY), {emqttb_metrics_rolling_average, KEY}).

-define(TICK_TIME, 100).
-define(MAX_WINDOW_SEC, 30). % Keep about 30 seconds of history
-define(DEFAULT_WINDOW, 5000).

-opaque metric_ref() :: tuple().

%%================================================================================
%% API funcions
%%================================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Declarative API:

-spec from_model(lee:model_key()) -> metric_id().
from_model(ModelKey) ->
  #mnode{metaparams = MPs} = lee_model:get(ModelKey, ?MYMODEL),
  ?m_attr(metric, id, MPs).

%% TODO:
-spec opstat_from_model(lee:model_key()) -> {metric_id(), metric_id()}.
opstat_from_model(Key) ->
  {from_model(Key ++ [avg_time]), from_model(Key ++ [pending])}.

-spec opstat(atom(), atom()) -> lee:namespace().
opstat(Group, Operation) ->
  #{ avg_time =>
       {[metric],
        #{ oneliner => "Average time of the operation"
         , metric_type => rolling_average
         , id => ?GROUP_OP_TIME(Group, Operation)
         , labels => [group, operation]
         , unit => "ms"
         }}
   , pending =>
       {[metric],
        #{ oneliner => "Number of pending operations"
         , metric_type => counter
         , id => ?GROUP_N_PENDING(Group, Operation)
         , labels => [group, operation]
         }}
   }.

-spec call_with_counter(metric_ref(), module(), atom(), list()) -> _.
call_with_counter({AvgTime, NPending}, Mod, Fun, Args) ->
  emqttb_metrics:counter_inc(NPending, 1),
  T0 = os:system_time(microsecond),
  try apply(Mod, Fun, Args)
  catch
    EC:Err ->
      EC(Err)
  after
    T = os:system_time(microsecond),
    emqttb_metrics:counter_dec(NPending, 1),
    emqttb_metrics:rolling_average_observe(AvgTime, T - T0)
  end.


%% Simple counters and gauges:

-spec new_counter(metric_id(), list()) -> metric_id().
new_counter(Key, PrometheusParams) ->
  ok = gen_server:call(?SERVER, {new_counter, Key, PrometheusParams}),
  Key.

-spec new_gauge(metric_id(), list()) -> metric_id().
new_gauge(Key, PrometheusParams) ->
  ok = gen_server:call(?SERVER, {new_counter, Key, PrometheusParams}),
  Key.

-spec counter_inc(metric_id(), integer()) -> ok.
counter_inc(Key, Delta) ->
  counters:add(persistent_term:get(?C(Key)), 1, Delta).

-spec counter_dec(metric_id(), integer()) -> ok.
counter_dec(Key, Delta) ->
  counters:sub(persistent_term:get(?C(Key)), 1, Delta).

-spec get_counter(metric_id()) -> integer().
get_counter(Key) ->
  counters:get(persistent_term:get(?C(Key)), 1).

-spec gauge_set(metric_id(), integer()) -> ok.
gauge_set(Key, Value) ->
  counters:put(persistent_term:get(?C(Key)), 1, Value).

%% Get the raw counter reference for fast access:
-spec gauge_ref(metric_id()) -> counters:counters_ref().
gauge_ref(Key) ->
  persistent_term:get(?C(Key)).

%% Rolling average

-spec new_rolling_average(metric_id(), list()) -> metric_id().
new_rolling_average(Key, PrometheusParams) ->
  ok = gen_server:call(?SERVER, {new_rolling_average, Key, PrometheusParams}),
  Key.

%% Fast update of rolling average of a value:
-spec rolling_average_observe(metric_id(), integer()) -> ok.
rolling_average_observe(Key, Val) ->
  Cnt = persistent_term:get(?RA(Key)),
  counters:add(Cnt, 1, Val), %% Update the sum
  counters:add(Cnt, 2, 1).   %% Update the number of samples

-spec get_rolling_average(metric_id()) -> integer().
get_rolling_average(Key) ->
  get_rolling_average(Key, ?DEFAULT_WINDOW).

-spec get_rolling_average(metric_id(), non_neg_integer()) -> integer().
get_rolling_average(Key, Window) ->
  case gen_server:call(?SERVER, {get_rolling_average, Key, Window}) of
    undefined ->
      error({missing_rolling_average, Key});
    Val ->
      Val
  end.

%%================================================================================
%% lee_metatype callbacks
%%================================================================================

names(_) ->
  [metric].

metaparams(metric) ->
  [ {mandatory, metric_type, typerefl:union([counter, gauge, rolling_average])}
  , {mandatory, id, metric_id()}
  , {mandatory, oneliner, string()}
  , {mandatory, labels, [atom()]}
  , {optional, unit, string()}
  ].

meta_validate(metric, Model) ->
  Ids = [begin
           #mnode{metaparams = #{id := Id}} = lee_model:get(Key, Model),
           Id
         end || Key <- lee_model:get_metatype_index(metric, Model)],
  case length(lists:usort(Ids)) =:= length(Ids) of
    false -> {["Metric IDs must be unique"], [], []};
    true  -> {[], [], []}
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
        { key        :: metric_id()
        , datapoints :: [#p{}]
        }).

-record(s,
        { counters        = [] :: [metric_id()]
        , rolling_average = [] :: [#r{}]
        }).

init([]) ->
  %% Create metrics defined in the model
  State = lists:foldl(
            fun(ModelKey, S) ->
                #mnode{metaparams = MPs} = lee_model:get(ModelKey, ?MYMODEL),
                PrometheusParams0 = case MPs of
                                      #{unit := Unit, id := MetricId, labels := Labels} ->
                                        [{unit, Unit}];
                                      #{id := MetricId, labels := Labels} ->
                                        []
                                    end,
                PrometheusParams = [ {help, list_to_binary(?m_attr(metric, oneliner, MPs))}
                                   , {labels, Labels}
                                   | PrometheusParams0
                                   ],
                case ?m_attr(metric, metric_type, MPs) of
                  counter ->
                    mk_counter(S, MetricId, PrometheusParams);
                  gauge ->
                    mk_counter(S, MetricId, PrometheusParams);
                  rolling_average ->
                    mk_rolling_average(S, MetricId, PrometheusParams)
                end
            end,
            #s{},
            lee_model:get_metatype_index(metric, ?MYMODEL)),
  %% Start timer:
  self() ! tick,
  {ok, State}.

handle_call({new_counter, Key, PrometheusParams}, _From, S) ->
  {reply, ok, mk_counter(S, Key, PrometheusParams)};
handle_call({new_rolling_average, Key, PrometheusParams}, _From, S) ->
  {reply, ok, mk_rolling_average(S, Key, PrometheusParams)};
handle_call({get_rolling_average, Key, Window}, _From, S) ->
  Ret = case lists:keyfind(Key, #r.key, S#s.rolling_average) of
          false -> undefined;
          RA    -> do_get_rolling_average(RA, Window)
        end,
  {reply, Ret, S};
handle_call(_Call, _From, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_Call, S) ->
  {noreply, S}.

handle_info(tick, S0) ->
  erlang:send_after(?TICK_TIME, self(), tick),
  S = collect_rolling_averages(S0),
  update_prometheus(S),
  {noreply, S};
handle_info(_, S) ->
  {noreply, S}.

terminate(_Reason, S) ->
  {ok, S}.

%%================================================================================
%% Internal functions
%%================================================================================

collect_rolling_averages(S = #s{rolling_average = RA}) ->
  S#s{rolling_average = lists:map(fun do_collect_rolling_average/1, RA)}.

do_get_rolling_average(#r{key = Key, datapoints = DP}, Window) ->
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

look_back(_StartTime, [A]) ->
  A;
look_back(StartTime, [A|Rest]) ->
  if A#p.time =< StartTime ->
      A;
     true ->
      look_back(StartTime, Rest)
  end.

do_collect_rolling_average(R0 = #r{key = K, datapoints = DP0}) ->
  DP = lists:droplast(DP0),
  R0#r{datapoints = [collect_datapoint(K) | DP]}.

collect_datapoint(K) ->
  Cnt = persistent_term:get(?RA(K)),
  #p{ sum = counters:get(Cnt, 1)
    , samples = counters:get(Cnt, 2)
    , time = os:system_time(millisecond)
    }.

%% Really ugly stuff, like everything about prometheus...
update_prometheus(#s{counters = CC, rolling_average = RR}) ->
  lists:foreach(
    fun(K = {Tag, Label}) ->
        LabelList = prom_labels(Label),
        prometheus_gauge_set(Tag, LabelList, get_counter(K))
    end,
    CC),
  lists:foreach(
    fun(R = #r{key = {Tag, Label}}) ->
        LabelList = prom_labels(Label),
        ok = prometheus_gauge_set(Tag, LabelList, do_get_rolling_average(R, ?DEFAULT_WINDOW))
    end,
    RR).

prometheus_gauge_set(Tag, LabelList, Val) ->
  try
    prometheus_gauge:set(Tag, LabelList, Val)
  catch
    EC:Err ->
      logger:error(#{EC => Err, tag => Tag, labels => LabelList})
  end.

mk_rolling_average(S = #s{rolling_average = RR}, Key = {Tag, _Labels}, PrometheusParams) ->
  case lists:keyfind(Key, #r.key, RR) of
    false ->
      prometheus_gauge:declare([{name, Tag}|PrometheusParams]),
      Cnt = counters:new(2, [write_concurrency]),
      persistent_term:put(?RA(Key), Cnt),
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
      gauge_set(Key, 0),
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
