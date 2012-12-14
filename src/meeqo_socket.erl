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

-module(meeqo_socket).

-export([start/0, start/1]).

-define(PORT, 6611).
-define(SOCKOPT, [binary,{active,true}]).
-define(MAXPROC, 2).

start() ->
    start(?PORT).

start(Port) when is_integer(Port) ->
    process_flag(trap_exit, true),
    {ok, LSock} = gen_tcp:listen(Port, ?SOCKOPT), 
    put(active, 0), % record the number of active processes
    active([LSock, self()]),
    loop(LSock).

loop(LSock) ->
    receive
        {accepted} ->
            case get(active) of
                N when N == ?MAXPROC -> put(accepting, false);
                _ -> active([LSock, self()])
            end;
        {'EXIT', _Pid, _Why} ->
            put(active, get(active) - 1),
            case get(accepting) of
                true -> pass;
                false -> active([LSock, self()])
            end;
        _ -> pass
    end,
    loop(LSock).
    
active(Args) ->
    spawn_link(fun() -> accept(Args) end),
    put(active, get(active) + 1),
    put(accepting, true).

accept([LSock, Pid]) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid ! {accepted},
    inet:setopts(Sock, ?SOCKOPT),
    recv(Sock).

recv(Sock) ->
    receive
        {tcp, Sock, Data} ->
            io:fwrite("~s", [Data]),
            io:fwrite("~p~n", [inet:peername(Sock)]),
            recv(Sock);
        {tcp_closed, Sock} -> ok
    end.
