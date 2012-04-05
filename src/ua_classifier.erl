%% Copyright 2012 Marc Worrell
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

-module(ua_classifier).

-behaviour(gen_server).
-author('Marc Worrell <marc@worrell.nl>').

-export([
    start/0, 
    start_link/0, 
    stop/0,
    
    classify/1
]).

%% gen_server callbacks
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, 
    terminate/2, code_change/3
]).

-define(SERVER, ?MODULE).
-record(state, {}).

-type ua_class() :: text | phone | tablet | desktop.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

%% @doc Classify an UA string
-spec classify( UA :: iolist() | undefined ) -> ua_class().
classify(undefined) ->
    desktop;
classify(UA) when is_binary(UA) ->
    UAs = [
        {phone, <<"iPhone">>},
        {tablet, <<"iPad">>}
    ],
    case lists:dropwhile(
                fun({_,X}) -> 
                    binary:match(UA, X) =:= nomatch
                end, 
                UAs)
    of
        [{Class,_}|_] -> Class;
        [] -> desktop
    end;
classify(UA) when is_list(UA) ->
    classify(iolist_to_binary(UA)).


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

code_change(_, _, _) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

