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

-module(meeqo_proxy).

-export([start/0, start/1]).

-define(PORT, 6611).
-define(SOCKOPT, [binary,{buffer,16777216}]).
-define(MAXPROC, 2).

start() ->
    start(?PORT).

start(Port) when is_integer(Port) ->
    process_flag(trap_exit, true),
    {ok, LSock} = gen_tcp:listen(Port, [{active,false} | ?SOCKOPT]),
    % The number of active processes
    put(active, 0), 
    % The pid of the process waiting for connection
    put(listener, nil), 
    active([LSock, self()]),
    loop(LSock).

loop(LSock) ->
    receive
        {accepted} ->
            case get(active) of
                % If the number of active processes reaches the limit, new
                % process won't be created temporarily.
                N when N == ?MAXPROC -> put(listener, nil);
                _ -> active([LSock, self()])
            end;
        {'EXIT', Pid, _Why} ->
            put(active, get(active) - 1),
            case get(listener) of
                nil -> active([LSock, self()]);
                % DO NOT forget that the listener would also fail.
                Pid -> active([LSock, self()]);
                _ -> pass
            end;
        _ -> pass
    end,
    loop(LSock).
    
active(Args) ->
    Pid = spawn_link(fun() -> listen(Args) end),
    put(active, get(active) + 1),
    put(listener, Pid).

listen([LSock, Pid]) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    % Inform the main process to create another listener.
    Pid ! {accepted},
    % Accept the message header and verify it.
    inet:setopts(Sock, [{active,once} | ?SOCKOPT]),
    receive
        {tcp, Sock, Data} ->
            case binary_to_list(Data) of
                % ASCII 10 is '\n'
                "send " ++ Dest ->
                    case address(string:strip(Dest, both, 10)) of
                        error -> pass;
                        Addr -> 
                            io:fwrite("send ~p~n", [Addr]),
                            recv(Sock)
                    end;
                "read " ++ From ->
                    case address(string:strip(From, both, 10)) of
                        error -> pass;
                        Addr -> io:fwrite("read ~p~n", [Addr])
                    end;
                "read" -> io:fwrite("read~n",[]);
                "read\n" -> io:fwrite("readn~n",[]);
                _ -> pass
            end
    end,
    gen_tcp:close(Sock).

recv(Sock) ->
    inet:setopts(Sock, [{active,once} | ?SOCKOPT]),
    receive
        {tcp, Sock, Data} ->
            io:fwrite("~s", [Data]),
            io:fwrite("~p~n", [inet:peername(Sock)]),
            recv(Sock);
        {tcp_closed, Sock} -> ok
    end.

address(Str) ->
    % The correct form should be like "192.168.1.100 8000".
    Tokens = string:tokens(Str, " "),
    case length(Tokens) of
        2 -> 
            [A, B] = Tokens,
            case inet_parse:address(A) of
                {ok, Ip} -> 
                    case string:to_integer(B) of
                        {P, []} when P > 0, P < 65536 -> {Ip, P};
                        _ -> error
                    end;
                _ -> error
            end;
        _ -> error
    end.

