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
-module(emqttb_group).

-behavior(gen_server).

%% API:
-export([start_link/1, ramp_up/1, ramp_down/1, foreach_children/2]).

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-export_type([]).

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API funcions
%%================================================================================

-spec start_link(emqttb:group()) -> {ok, pid()}.
start_link(GroupId) ->
  gen_server:start_link({local, GroupId}, ?MODULE, [GroupId], []).

%% Add a child to the group
-spec ramp_up(emqttb:group()) -> ok.
ramp_up(_Id) ->
  ok.

%% Remove a child from the group
-spec ramp_down(emqttb:group()) -> ok.
ramp_down(_Id) ->
  ok.

%%================================================================================
%% behavior callbacks
%%================================================================================

-record(s,
        { behavior :: module()
        , pids :: queue:queue(pid())
        }).

init([Name, Scenario, Behavior]) ->
  process_flag(trap_exits, true),
  logger:info("Starting group ~p", [Name]),
  {ok, #s{ behavior = Behavior
         , pids = queue:new()
         }}.

handle_call(_, _, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_, S) ->
  {notrepy, S}.

terminate(_Reason, State) ->
  ok.

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================

-spec foreach_children(fun((pid()) -> _), pid() | atom()) -> ok.
foreach_children(Fun, Id) when is_atom(Id) ->
  GL = whereis(Id),
  is_pid(GL) orelse throw({group_is_not_alive, Id}),
  foreach_children(Fun, GL);
foreach_children(Fun, GL) ->
  PIDs = erlang:processes(),
  Go = fun(Pid) ->
           case process_info(Pid, [group_leader]) of
             GL -> Fun(Pid);
             _  -> ok
           end
       end,
  lists:foreach(Go, PIDs).
