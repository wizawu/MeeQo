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
                  {recbuf, ?PROXY_RCVBUF},
                  {sndbuf, ?PROXY_SNDBUF}
                 ]).

start_link([SysTbl]) ->
    [Port] = ets:lookup(SysTbl, [port]),
    {ok, LSock} = gen_tcp:listen(Port, ?SOCKOPT),
    ets:insert(SysTbl, {?MODULE, self()}),
    process_flag(trap_exit, true),
    new_listener([LSock, SysTbl]),
    loop([LSock, SysTbl]).

new_listener([LSock, SysTbl]) ->
    Pid = spawn_link(fun() -> listen(self(), LSock, SysTbl) end),
    put(listener, Pid).

loop(Args) ->
    receive
        accepted -> new_listener(Args);
        {'EXIT', Pid, _Why} ->
            case get(listener) of
                Pid ->
                    % If listener fails, warn and new another.
                    Format = "meeqo_proxy listener ~w failed.",
                    error_logger:error_msg(Format, [Pid]),
                    timer:sleep(200),
                    new_listener(Args);
                _ -> ok
            end
    end,
    loop(Args).

listen(Loop, LSock, SysTbl) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    % Inform the main loop to create a new listener.
    Loop ! accepted,
    % Accept the first message and identify the datagram type.
    {ok, Data} = gen_tcp:recv(Sock, 0),
    case binary_part(Data, 0, 5) of
        <<"tweet">> -> tweet_loop(Sock, Data, SysTbl);
        _ ->
            self() ! {tcp, Sock, Data},
            read_send_loop(Sock, SysTbl)
    end.

read_send_loop(Sock, SysTbl) ->
    receive
        {tcp, Sock, Data} ->
            case decode_sr(Data) of
                read ->
                    [Inbox] = meeqo:info(SysTbl, [meeqo_inbox]),
                    Msg = read(Inbox, read),
                    gen_tcp:send(Sock, Msg);
                {read, Addr} ->
                    [Inbox] = meeqo:info(SysTbl, [meeqo_inbox]),
                    Msg = read(Inbox, {read, Addr}),
                    gen_tcp:send(Sock, Msg);
                {send, Addr, Bin} ->
                    Msg = case check(Bin) of
                        {true, X} -> X;
                        false -> send_loop(Sock, Bin)
                    end,
                    MsgRef = make_ref(),
                    ets:insert(meeqo_locker, {MsgRef, Msg}),
                    send(Addr, MsgRef, SysTbl),
                    gen_tcp:send(Sock, <<"ok">>)
            end,
            inet:setopts(Sock, [{active, once}]),
            read_send_loop(Sock, SysTbl);
        {tcp_closed, Sock} -> ok
    end.

send_loop(Sock, SoFar) ->
    inet:setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, New} ->
            Bin = <<SoFar/binary, New/binary>>,
            case check(Bin) of
                {true, Msg} -> Msg;
                false -> send_loop(Sock, Bin)
            end
    end.

check(Bin) ->
    Len = byte_size(Bin),
    {Pos, 1} = binary:match(binary_part(Bin, 1, Len-1), <<"[">>),
    L = (Pos - 1) * 8,
    M = (Len - L - L - 4) * 8,
    N = (Len - L - L - 5) * 8,
    case Bin of
        <<"[", A:L, "[", X:M/bitstring, "]", B:L, "]">> ->
            case A == B of
                true -> {true, X};
                false -> false
            end;
        <<"[", A:L, "[", X:N/bitstring, "]", B:L, "]\n">> ->
            case A == B of
                true -> {true, X};
                false -> false
            end;
        _ -> false
    end.
        
% Send request to meeqo_inbox to get the key to the message in meeqo_locker.
read(Inbox, Request) ->
    case gen_server:call(Inbox, Request) of
        nil -> <<>>;
        MsgRef when is_reference(MsgRef) ->
            [{MsgRef, Msg}] = ets:lookup(meeqo_locker, MsgRef),
            ets:delete(meeqo_locker, MsgRef),
            Msg
    end.

send(Addr, Msg, SysTbl) ->
    [PipeRack] = meeqo:info(SysTbl, [meeqo_piperack]),
    Pipe = case ets:lookup(PipeRack, Addr) of
        [] ->
            meeqo_pipe:start_link([SysTbl, Addr]);
        [{Addr, Pid}] ->
            case is_process_alive(Pid) of
                true -> Pid;
                false ->
                    ets:delete(PipeRack, Addr),
                    meeqo_pipe:start_link([SysTbl, Addr])
            end
    end,
    ets:insert(PipeRack, {Addr, Pipe}),
    gen_fsm:send_event(Pipe, {send, Msg}).

% Decode single read/send request into Elang data type.
decode_sr(Bin) ->
    Len = bit_size(Bin) - 40,
    Len1 = Len - 8,
    case Bin of
        <<"read">> -> read;
        % The ending CF is allowed, so we can use MeeQo via tools like Netcat.
        <<"read\n">> -> read;
        <<"read ", X:Len1/bitstring, "\n">> ->
            {read, address(binary_to_list(X))};
        <<"read ", X:Len/bitstring>> ->
            {read, address(binary_to_list(X))};
        <<"send ", X:Len/bitstring>> ->
            % The address and message are separated by a whitespace.
            case binary:match(X, <<" ">>) of
                {Pos, 1} ->
                    Addr = binary_part(X, 0, Pos),
                    Msg = binary_part(X, Pos+1, byte_size(X)-Pos-1),
                    {send, address(binary_to_list(Addr)), Msg};
                nomatch -> error
            end;
        _ -> error
    end.

tweet_loop(Sock, Prev, SysTbl) ->
    inet:setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, Data} ->
            case split_tweets(<<Prev/binary, Data/binary>>) of
                {[], Rest} -> tweet_loop(Sock, Rest, SysTbl);
                {Msgs, Rest} ->
                    Fun = fun({Addr, Msg}) ->
                        MsgRef = make_ref(),
                        ets:insert(meeqo_locker, {MsgRef, Msg}),
                        send(Addr, MsgRef, SysTbl)
                    end,
                    lists:map(Fun, Msgs),
                    tweet_loop(Sock, Rest, SysTbl)
            end;
        {tcp_closed, Sock} -> ok
    end.

% Split the bytes in the buffer into different tweets.
split_tweets(Bin) -> split_tweets(Bin, <<>>, []).

split_tweets(<<>>, Rest, Tweets) -> {lists:reverse(Tweets), Rest};
split_tweets(Bin, Part, Tweets) ->
    <<H, T/binary>> = Bin,
    case H of
        % 0 is null character, which is used to separate tweets.
        0 ->
            case Part of
                <<>> -> split_tweets(T, <<>>, Tweets);
                _ -> split_tweets(T, <<>>, [decode_tw(Part)|Tweets])
            end;
        _ ->
            split_tweets(T, <<Part/binary, H>>, Tweets)
    end.

% Decode single tweet into Erlang data type.
decode_tw(Bin) ->
    Len = bit_size(Bin) - 48,
    case Bin of
        <<"tweet ", X:Len/bitstring>> ->
            % The address and message are separated by a whitespace.
            case binary:match(X, <<" ">>) of
                {Pos, 1} ->
                    Addr = binary_part(X, 0, Pos),
                    Msg = binary_part(X, Pos+1, byte_size(X)-Pos-1),
                    {address(binary_to_list(Addr)), Msg};
                nomatch -> error
            end;
        _ -> error
    end.

% address(Str) -> {ip(), port()} | error
% e.g. address("192.168.1.100:8000") -> {{192,168,1,100}, 8000}
address(Str) ->
    case string:rchr(Str, $:) of
        0 -> error;
        X ->
            A = string:substr(Str, 1, X-1),
            B = string:substr(Str, X+1),
            case inet_parse:address(A) of
                {ok, Ip} -> 
                    case string:to_integer(B) of
                        {Port, []} when Port > 0, Port < 65536 -> 
                            {Ip, Port};
                        _ -> error
                    end;
                _ -> error
            end
    end.

