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

-export([start_link/1]).

-include("meeqo_config.hrl").

-define(SOCKOPT, [binary, {active, false},
                  {recbuf, ?RCVBUF},
                  {sndbuf, ?SNDBUF}
                 ]).

start_link([SysTbl, Port]) when is_integer(Port) ->
    ets:insert(SysTbl, {?MODULE, self()}),
    {ok, LSock} = gen_tcp:listen(Port, ?SOCKOPT),
    process_flag(trap_exit, true),
    Args = [LSock, 
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

listen([LoopPid, LSock]) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    % Inform the main loop to create a new listener.
    LoopPid ! {accepted},
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

send_read_mode(Sock) ->


tweet_mode(Sock) ->
    inet:setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, Data} ->
            
send(Addr, Msg) ->
    gen

read(Inbox, Locker) ->
    case gen_server:call(Inbox, read) of
        nil -> <<>>;
        MsgRef when is_reference(MsgRef) ->
            [{MsgRef, Msg}] = ets:lookup(Locker, MsgRef),
            ets:delete(Locker, MsgRef),
            Msg
    end.

read(Inbox, Locker, Addr) ->
    case gen_server:call(Inbox, {read, Addr}) of
        nil -> <<>>;
        MsgRef when is_reference(MsgRef) ->
            [{MsgRef, Msg}] = ets:lookup(Locker, MsgRef),
            ets:delete(Locker, MsgRef),
            Msg
    end.

decode(read) ->

% Split the bytes in the buffer into different tweets.
split_tweets(Bin) -> split_tweets(Bin, <<>>, []).

split_tweets(<<>>, Rest, Tweets) -> {lists:reverse(Tweets), Rest};
split_tweets(Bin, Part, Tweets) ->
    <<H, T/binary>> = Bin,
    case H of
        % 0 is null character, which is used to separate tweets.
        0 ->
            case Rest of
                <<>> -> split_tweets(T, <<>>, Tweets);
                _ -> split_tweets(T, <<>>, [Rest|Tweets])
            end;
        _ ->
            split_tweets(T, <<Part/binary, H>>, Tweets)
    end.

% address(Str) -> {ip(), port()} | error
% e.g. address("192.168.1.100 8000") -> {{192,168,1,100}, 8000}
address(Str) ->
    Tokens = string:tokens(Str, " "),
    case length(Tokens) of
        2 -> 
            [A, B] = Tokens,
            case inet_parse:address(A) of
                {ok, Ip} -> 
                    case string:to_integer(B) of
                        {Port, []} when Port > 0, Port < 65536 -> 
                            {Ip, Port};
                        _ -> error
                    end;
                _ -> error
            end;
        _ -> error
    end.

