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

-module(meeqo_pipe).

-behaviour(gen_fsm).

-export([start/1]).

-export([init/1, empty/2, ready/2, full/2,
         handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-import(lists, [reverse/1]).

-record(state, {count = 0, mll = <<>>, body = <<>>, msgs = []}).

-include("meeqo_config.hrl").

-define(SOCKOPT, [binary, {active, false}, {packet, 4},
                  {recbuf, ?PIPE_BUF},
                  {sndbuf, ?PIPE_BUF}
                 ]).

start(Args) ->
    gen_fsm:start(?MODULE, Args, []).

init([SysTbl, {Ip, Port}]) ->
    % When message is sent from one MeeQo instance(A) to another(B), it is sent
    % from A's meeqo_pipe to B's meeqo_sink. And the port of meeqo_sink is
    % always the port of meeqo_proxy on the same instance plus 1.
    {ok, Sock} = gen_tcp:connect(Ip, Port+1, ?SOCKOPT),
    [ProxyPort, Sluice] = meeqo:info(SysTbl, [port, meeqo_sluice]),
    put(sock, Sock),
    put(proxy_port, ProxyPort),
    put(sluice, Sluice),
    {ok, empty, #state{}}.

empty(timeout, State) ->
    Sock = get(sock),
    ok = gen_tcp:close(Sock),
    io:format("meeqo_pipe ~w idles and exits.~n", [self()]),
    {stop, 'IDLE', State};
% {send, NewMsg} is cast from meeqo_proxy. Before the pipe is allowed to send,
% the messages will be packed into a parcel.
empty({send, NewMsg}, State) ->
    % Inform meeqo_sluice that the pipe has messages to send.
    gen_server:cast(get(sluice), {queue, self()}),
    append(State, NewMsg).

ready({send, NewMsg}, State) ->
    append(State, NewMsg).

full({send, NewMsg}, State) ->
    #state{msgs = Msgs} = State,
    {next_state, full, State#state{msgs = [NewMsg|Msgs]}}.

% 'send' is cast from meeqo_sluice, which means the pipe can send a parcel now.
handle_event(send, _StateName, State) ->
    #state{mll = MLL, body = Body, msgs = Msgs} = State,
    ProxyPort = get(proxy_port),
    MLLBytes = byte_size(MLL),
    Parc = <<ProxyPort:16, MLLBytes:32, MLL/binary, Body/binary>>,
    Sock = get(sock),
    gen_tcp:send(Sock, Parc),
    {ok, <<"ok">>} = gen_tcp:recv(Sock, 0),
    Sluice = get(sluice),
    case Msgs of
        [] ->
            % If there is no more messages, the TCP connection should be closed.
            gen_server:cast(Sluice, sent),
            {next_state, empty, #state{}, 30000};
        _ ->
            gen_server:cast(Sluice, {sent, self()}),
            % The lastest message is on the head, so reverse the list before
            % packing.
            concat(#state{msgs = reverse(Msgs)})
    end;
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

% Pack the available messages as many as possible.
concat(#state{msgs = []} = State) ->
    {next_state, ready, State};
concat({state, Count, MLL, Body, [Msg|More]}) ->
    {NewMLL, NewBody} = append_to_binary(MLL, Body, Msg),
    State = {state, Count+1, NewMLL, NewBody, More},
    case byte_size(NewMLL) + byte_size(NewBody) of
        X when X >= ?PARC_MAX_MEM ->
            {next_state, full, State#state{msgs = reverse(More)}};
        _ -> case (Count + 1) of
            ?PARC_MAX_MSG ->
                {next_state, full, State#state{msgs = reverse(More)}};
            _ -> concat(State)
        end
    end.

% When the parcel is not full, append the new message to it.
append({state, Count, MLL, Body, Msgs}, NewMsg) ->
    {NewMLL, NewBody} = append_to_binary(MLL, Body, NewMsg),
    State = {state, Count+1, NewMLL, NewBody, Msgs},
    case byte_size(NewMLL) + byte_size(NewBody) of
        X when X >= ?PARC_MAX_MEM ->
            {next_state, full, State};
        _ -> case (Count + 1) of
            ?PARC_MAX_MSG -> {next_state, full, State};
            _ -> {next_state, ready, State}
        end
    end.

append_to_binary(MLL, Body, MsgRef) ->
    [{_, Msg}] = ets:lookup(meeqo_locker, MsgRef),
    L = encode_length(byte_size(Msg)),
    NewMLL = <<MLL/binary, L/binary>>,
    NewBody = <<Body/binary, Msg/binary>>,
    {NewMLL, NewBody}.

encode_length(L) when is_integer(L) ->
    if L < 255 ->
        <<L>>;
    true ->
        <<255, L:40>>
    end.

