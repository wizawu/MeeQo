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

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {listen, lsock, systbl}).

-include("meeqo_config.hrl").

-define(SOCKOPT, [binary, {active, false},
                  {recbuf, ?PROXY_RCVBUF},
                  {sndbuf, ?PROXY_SNDBUF}
                 ]).

start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).

init([SysTbl]) ->
    [Port] = meeqo:info(SysTbl, [port]),
    {ok, LSock} = gen_tcp:listen(Port, ?SOCKOPT),
    ets:insert(SysTbl, {?MODULE, self()}),
    process_flag(trap_exit, true),
    Self = self(),
    Listen = spawn_link(fun() -> listen(Self, LSock, SysTbl) end),
    {ok, {state, Listen, LSock, SysTbl}}.

handle_call(accepted, _From, State) ->
    {state, _, LSock, SysTbl} = State,
    Self = self(),
    Listen = spawn_link(fun() -> listen(Self, LSock, SysTbl) end),
    {reply, ok, State#state{listen = Listen}};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Why}, State) ->
    {state, _, LSock, SysTbl} = State,
    NewState = case State#state.listen of
        Pid ->
            % If listener fails, warn and new another.
            Format = "meeqo_proxy listener ~w failed.",
            error_logger:error_msg(Format, [Pid]),
            timer:sleep(1000),
            Self = self(),
            Listen = spawn_link(fun() -> listen(Self, LSock, SysTbl) end),
            State#state{listen = Listen};
        _ -> State
    end,
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%%  Internal Functions
%%-----------------------------------------------------------------------------
listen(Svr, LSock, SysTbl) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    % Inform the server to create a new listener.
    ok = gen_server:call(Svr, accepted),
    % Accept the first message and identify the datagram type.
    {ok, Data} = gen_tcp:recv(Sock, 0),
    self() ! {tcp, Sock, Data},
    case binary_part(Data, 0, 4) of
        <<"twee">> -> 
            tweet_loop(Sock, <<>>, SysTbl);
        _ ->
            read_send_loop(Sock, SysTbl)
    end.

read_send_loop(Sock, SysTbl) ->
    receive
        {tcp, Sock, Data} ->
            case decode_sr(Data) of
                read ->
                    [Inbox] = meeqo:info(SysTbl, [meeqo_inbox]),
                    Msg = read(Inbox, read),
                    % If get(cf) is true, Data must be <<"read\n">>. So the
                    % request may come from some CLI. A "\n" should be appended
                    % by the result.
                    case get(cf) of 
                        true -> gen_tcp:send(Sock, [Msg, <<"\n">>]);
                        _ -> gen_tcp:send(Sock, Msg)
                    end;
                {read, Addr} ->
                    [Inbox] = meeqo:info(SysTbl, [meeqo_inbox]),
                    Msg = read(Inbox, {read, Addr}),
                    case get(cf) of 
                        true -> gen_tcp:send(Sock, [Msg, <<"\n">>]);
                        _ -> gen_tcp:send(Sock, Msg)
                    end;
                {send, Addr, Bin} when is_tuple(Addr) ->
                    Msg = case check(Bin) of
                        {true, X} -> X;
                        false -> send_loop(Sock, Bin)
                    end,
                    MsgRef = make_ref(),
                    ets:insert(meeqo_locker, {MsgRef, Msg}),
                    send(Addr, MsgRef, SysTbl),
                    % The client should listen "ok" from meeqo_proxy in order to
                    % separate two neighbouring send-datagrams.
                    case get(cf) of
                        true -> gen_tcp:send(Sock, <<"ok\n">>);
                        _ -> gen_tcp:send(Sock, <<"ok">>)
                    end
            end,
            inet:setopts(Sock, [{active, once}]),
            read_send_loop(Sock, SysTbl);
        {tcp_closed, Sock} -> ok
    end.

% Receive complete data in one send-datagram.
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

% Check whether the message part in send-datagram is complete.
check(Bin) ->
    Len = byte_size(Bin),
    Tail = binary_part(Bin, 1, Len-1),
    % Position of the second "[".
    Pos = element(1, binary:match(Tail, <<"[">>)) + 1,
    % Bit size of the infilling between first two "[", which is also between the
    % last two "]".
    L = (Pos - 1) * 8,
    M = (Len - 4) * 8 - L - L,
    % An extra CF at the end is allowed, so we can use MeeQo via tools like
    % Netcat.
    N = (Len - 5) * 8 - L - L,
    case Bin of
        <<"[", A:L, "[", X:M/bitstring, "]", B:L, "]">> ->
            case A == B of
                true -> 
                    put(cf, false),
                    {true, X};
                false -> false
            end;
        <<"[", A:L, "[", X:N/bitstring, "]", B:L, "]\n">> ->
            case A == B of
                true -> 
                    put(cf, true),
                    {true, X};
                false -> false
            end;
        _ -> false
    end.

% Send a read-request to meeqo_inbox.
read(Inbox, Request) ->
    case gen_server:call(Inbox, Request) of
        nil -> <<>>;
        MsgRef when is_reference(MsgRef) ->
            [{MsgRef, Msg}] = ets:lookup(meeqo_locker, MsgRef),
            ets:delete(meeqo_locker, MsgRef),
            Msg
    end.

% Send a send-message-event to meeqo_pipe.
send(Addr, Msg, SysTbl) ->
    [PipeRack] = meeqo:info(SysTbl, [meeqo_piperack]),
    Pipe = case ets:lookup(PipeRack, Addr) of
        [] ->
            % Create a meeqo_pipe for each address.
            {ok, Pid} = meeqo_pipe:start([SysTbl, Addr]),
            ets:insert(PipeRack, {Addr, Pid}),
            Pid;
        [{Addr, OldPid}] ->
            case is_process_alive(OldPid) of
                true -> OldPid;
                false ->
                    {ok, Pid} = meeqo_pipe:start([SysTbl, Addr]),
                    ets:insert(PipeRack, {Addr, Pid}),
                    Pid
            end
    end,
    gen_fsm:send_event(Pipe, {send, Msg}).

% Decode single read/send request into Elang data type.
decode_sr(Bin) ->
    Len = bit_size(Bin) - 40,
    Len1 = Len - 8,
    case Bin of
        <<"read">> ->
            put(cf, false),
            read;
        % The ending CF is allowed, so we can use MeeQo via tools like Netcat.
        <<"read\n">> ->
            put(cf, true),
            read;
        <<"read ", X:Len1/bitstring, "\n">> ->
            put(cf, true),
            {read, address(binary_to_list(X))};
        <<"read ", X:Len/bitstring>> ->
            put(cf, false),
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
                    % The incomplete tweet at the end will be attached to the 
                    % next tweet-datagram.
                    inet:setopts(Sock, [{active, once}]),
                    tweet_loop(Sock, Rest, SysTbl)
            end;
        {tcp_closed, Sock} -> ok
    end.

% Split the bytes in the buffer into different tweets.
split_tweets(Bin) -> split_tweets(Bin, <<>>, []).

split_tweets(<<>>, Rest, Tweets) -> {lists:reverse(Tweets), Rest};
split_tweets(Bin, Part, Tweets) ->
    <<H:8/integer, T/binary>> = Bin,
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
decode_tw(Bin0) ->
    Bin = case binary:first(Bin0) of
        10 -> binary_part(Bin0, 1, byte_size(Bin0)-1);
        _ -> Bin0
    end,
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

