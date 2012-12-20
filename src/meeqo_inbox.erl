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
    % Create a table to store the minimum timestamp of all sessions.
    % The K-V is {ts(), addr()}.
    Tid = ets:new(anonym, [ordered_set]),
    process_flag(trap_exit, true),
    {ok, #state{tid = Tid, uts = 0, unread = 0}}.

handle_call(read, From, State) ->
    #state{tid = Tid, unread = Unread} = State,
    case Unread of
        0 -> {reply, nil, State};
        N when N > 0 ->
            % Keep in mind the lookup will return a list.
            [{_, Addr}] = ets:lookup(Tid, ets:first(Tid)),
            handle_call({read, Addr}, From, State)
    end;
handle_call({read, Addr}, _From, State) ->
    #state{tid = Tid, unread = Unread} = State,
    Reply = case get(Addr) of
        undefined -> nil;
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                false -> 
                    % Delete lazily.
                    erase(Addr),
                    nil;
                true ->
                    case gen_server:call(Pid, read) of
                        nil -> nil;
                        {Msg, Uts, nil} ->
                            ets:delete(Tid, Uts),
                            Msg;
                        {Msg, Uts, NextUts} ->
                            % Update the queueing order of Addr.
                            ets:delete(Tid, Uts),
                            ets:insert(Tid, {NextUts, Addr}),
                            Msg
                    end
            end
    end,
    NewUnread = (case Reply of nil -> Unread; _ -> Unread - 1 end),
    {reply, Reply, State#state{unread = NewUnread}};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({save, Addr, Msg}, State) ->
    #state{tid = Tid, uts = Uts, unread = Unread} = State,
    Ses = case get(Addr) of
        undefined ->
            % Create a meeqo_session for each address.
            {ok, Pid} = meeqo_session:start_link([]),
            put(Addr, Pid),
            Pid;
        Pid when is_pid(Pid) -> 
            % Since keys are deleted lazily, it is possible that the process
            % is already dead.
            case is_process_alive(Pid) of
                true -> Pid;
                false ->
                    {ok, NewPid} = meeqo_session:start_link([]),
                    put(Addr, NewPid),
                    NewPid
            end
    end,
    % In case that when the message queue of the session was just emptied, its
    % entry is not in the table, a new {uts(), addr()} should be inserted.
    SesMinUts = gen_server:call(Ses, {save, Msg, Uts + 1}),
    case ets:member(Tid, SesMinUts) of
        true -> ok;
        false -> ets:insert(Tid, {SesMinUts, Addr})
    end,
    {noreply, State#state{uts = Uts + 1, unread = Unread + 1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, 'IDLE'}, State) ->
    io:format("~w idles and exits.~n", [_Pid]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

