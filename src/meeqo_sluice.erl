%%
%%  Copyright (c) 2012 Hualiang Wu <wizawu@gmail.com>
%%
%%  Permission is hereby granted, free of charge, to any person obtaining a copy
%%  of this software and associated documentation files (the "Software"), to
%%  deal in the Software without restriction, including without limitation the
%%  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%%  sell copies of the Software, and to permit persons to whom the Software is
%%  furnished to do so, subject to the following conditions:
%%
%%  The above copyright notice and this permission notice shall be included in
%%  all copies or substantial portions of the Software.
%%
%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%%  IN THE SOFTWARE.
%%

-module(meeqo_sluice).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tid, ts, act}).

-include("meeqo_config.hrl").

start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).

init([SysTbl]) ->
    ets:insert(SysTbl, {?MODULE, self()}),
    % Create a table to define the order of pipes to be actived.
    Tid = ets:new(anonym, [ordered_set]),
    {ok, #state{tid = Tid, ts = 0, act = 0}}.

handle_cast({queue, Pid}, State) ->
    #state{tid = Tid, ts = Ts, act = N} = State,
    if N < ?MAX_ACT_PIPES ->
        gen_fsm:send_event(Pid, send),
        {noreply, State#state{act = N + 1}};
    true ->
        ets:insert(Tid, {Ts + 1, Pid}),
        {noreply, State#state{ts = Ts + 1}}
    end;
handle_cast({sent, Pid}, State) ->
    #state{tid = Tid, ts = Ts} = State,
    ets:insert(Tid, {Ts + 1, Pid}),
    % Active another pipe to send message and delete its entry.
    Key = ets:first(Tid),
    [{_, ActPid}] = ets:lookup(Tid, Key),
    ets:delete(Tid, Key),
    gen_fsm:send_all_state_event(ActPid, send),
    {noreply, State#state{ts = Ts + 1}};
handle_cast(sent, State) ->
    #state{tid = Tid, act = N} = State,
    case ets:first(Tid) of
        '$end_of_table' ->
            {noreply, State#state{act = N - 1}};
        Key ->
            [{_, Pid}] = ets:lookup(Tid, Key),
            ets:delete(Tid, Key),
            gen_fsm:send_event(Pid, send),
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

