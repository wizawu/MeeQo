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

-module(meeqo_inbox).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tid, uts, unread}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init([WorkerTable]) ->
    ets:insert(WorkerTable, {?MODULE, self()}),
    % Create a table to store the smallest timestamp of all sessions.
    % The K-V is {ts(), addr()}.
    Tid = ets:new(anonym, [ordered_set]),
    process_flag(trap_exit, true),
    {ok, #state{tid=Tid, uts=0, unread=0}}.

handle_call({save, Addr, Msg}, _From, State) ->
    #state{tid=Tid, uts=Uts, unread=Unread} = State,
    case get(Addr) of
        undefined ->
            Pid = spawn_link(fun() -> session() end),
            put(Addr, Pid),
            Pid ! {save, Msg, Uts + 1};
        Pid when is_pid(Pid) ->
            Pid ! {save, Msg, Uts + 1}
    end,
    {reply, ok, #state{tid=Tid, uts=Uts+1, unread=Unread+1}}.

handle_call({read}, _From, State) ->
    #state{tid=Tid, uts=Uts, unread=Unread} = State,

handle_call({read, Addr}, _From, State) ->
    #state{tid=Tid, uts=Uts, unread=Unread} = State,
    case get(Addr) of
        undefined -> Reply = nil;
        Pid when is_pid(Pid) ->
            
    

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


session() ->
    receive
        {save, Msg, Uts, From} ->
            % get(ts) is the timestamp of the process, while Uts is the unified
            % timestamp of inbox.
            Ts = case get(ts) of
                undefined -> put(ts, 1), 1;
                X -> put(ts, X + 1), X + 1
            end,
            Top = case get(top) of
                % If get(top) is undefined, either the process is just newed or
                % all the former messages are read.
                undefined -> put(top, Ts), Ts;
                Y -> Y
            end,
            put(Ts, {Msg, Uts}),
            From ! element(2, get(Top));
        {read, From} ->
            Top = get(top),
            case get(Top) of
                {Msg, _} -> 
                    % Keep in mind to free the process memory.
                    erase(Top),
                    put(top, Top + 1),
                    Next = case get(Top + 1) of
                        {_, Uts} -> Uts;
                        undefined ->
                            % In this situation, there is no more message.
                            erlang:send_after(30000, self(), idle),
                            nil
                    end,
                    % Reply with the message wanted and oldest timestamp used to
                    % update the queueing order in inbox.
                    From ! {Msg, Next};
                undefined ->
                    From ! nil
            end;
        idle ->
            % If there is no unread message, exit.
            case get(get(top)) of
                undefined -> exit('IDLE');
                _ -> ok
            end
    end,
    session().

