%%--------------------------------------------------------------------
%%Copyright (c) 2022 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(emqttb_scenario_make_docs).

-behavior(emqttb_scenario).


%% behavior callbacks:
-export([ name/0
        , model/0
        , run/0
        ]).

-include("emqttb.hrl").
-include_lib("typerefl/include/types.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% behavior callbacks
%%================================================================================

name() ->
  'make-docs'.

model() ->
  #{ external_doc =>
       {[value, cli_param],
        #{ oneliner => "Path to the external documentation source file"
         , type => string()
         , cli_operand => "src"
         }}
   }.

run() ->
  DocumentedMTs = [cli_param, os_env, system_wide_conf, map, value],
  External = emqttb_scenario:my_conf([external_doc]),
  lee_doc:make_docs(?MYMODEL, #{ metatypes => DocumentedMTs
                               , run_pandoc => true
                               , doc_xml => External
                               }).
