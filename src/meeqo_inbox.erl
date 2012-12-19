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

-export([start_link/0]).


start_link(WorkerTable) ->
    ets:insert(WorkTable, {?MODULE, self()}),
    Tid = ets:new(anonym, [ordered_set]),
    put(ts, 0),
    put(tid, Tid),
    process_flag(trap_exit, true),
    loop().


loop(Tid) ->
    receive
        {save, {Addr, Msg}} -> save(Addr, Msg);
        {read, Pid} ->
            case lists:keyfind(size, 1, ets:info(Tid)) of
                {size, 0} -> Pid ! nil;
                {size, _} ->
                    [{_Ts, Addr}] = ets:lookup(Tid, ets:first(Tid)),
                    read(Addr, Pid)
            end;
        {read, Addr, Pid} -> read(Addr, Pid);
        {'EXIT', _Pid, 'IDLE'} -> ok
    end,
    loop(Tid).


save(Addr, Msg) ->
    case get(Addr) of
        undefined ->
            Pid = spawn_link(fun() -> session() end),
            put(Addr, Pid);
        Pid when is_pid(Pid) -> ok
    end,
    Ts = get(ts) + 1,
    put(ts, Ts),
    Pid ! {save, Msg, Ts},
    receive
        TopTs when is_integer(TopTs) ->
            ets:insert(get(tid), {TopTs, Addr})
    end.


read(Addr, Pid) ->
    case get(Addr) of
        undefined -> Pid ! nil;
            SesPid ->
                    SesPid ! {read, self()},
                    receive
                        nil -> Pid ! nil;
                        {Msg, nil} ->
                            ets:delete(Tid, Addr),

                        {Msg, Ts} ->
                            ets:insert(Tid, {Addr, Ts}),
                    end


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

