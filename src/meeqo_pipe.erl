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

-export([start_link/1]).

-export([init/1, empty/2, ready/2, full/2,
         handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-record(state, {count = 0, mll = <<>>, body = <<>>, msgs = []}).

-include("meeqo_config.hrl").

-define(SOCKOPT, [binary, {active, false}, {packet, 4},
                  {recbuf, ?PIPE_BUF},
                  {sndbuf, ?PIPE_BUF}
                 ]).

start_link(Args) ->
    gen_fsm:start_link(?MODULE, Args, []).

init([Sluice, Ip, Port, ProxyPort]) ->
    % Port of meeqo_sink is always port of proxy add 1.
    {ok, Sock} = gen_tcp:connect(Ip, Port+1, ?SOCKOPT),
    put(sock, Sock),
    put(sluice, Sluice),
    put(proxyPort, ProxyPort),
    {ok, empty, #state{}}.

empty(timeout, State) ->
    Sock = get(sock),
    ok = gen_tcp:close(Sock),
    {stop, 'IDLE', State};
empty({send, NewMsg}, State) ->
    gen_server:cast(get(sluice), {queue, self()}),
    append(State, NewMsg).

ready({send, NewMsg}, State) ->
    append(State, NewMsg).

full({send, NewMsg}, State) ->
    #state{msgs = Msgs} = State,
    {next_state, full, State#state{msgs = [NewMsg|Msgs]}}.

handle_event(send, _StateName, State) ->
    #state{mll = MLL, body = Body, msgs = Msgs} = State,
    ProxyPort = get(proxyPort),
    MLLBytes = byte_size(MLL),
    Parc = <<ProxyPort:16, MLLBytes:32, MLL/binary, Body/binary>>,
    Sock = get(sock),
    gen_tcp:send(Sock, Parc),
    {ok, <<"ok", _/binary>>} = gen_tcp:recv(Sock, 0),
    Sluice = get(sluice),
    case Msgs of
        [] ->
            gen_server:cast(Sluice, sent),
            {next_state, empty, #state{}, 30000};
        _ ->
            gen_server:cast(Sluice, {sent, self()}),
            concat(#state{msgs = Msgs})
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

concat(#state{msgs = []} = State) ->
    {next_state, ready, State};
concat({Count, MLL, Body, [Msg|More]}) ->
    L = meeqo_msg:encode_length(byte_size(Msg)),
    NewMLL = <<MLL/binary, L/binary>>,
    NewBody = <<Body/binary, Msg/binary>>,
    NewState = {Count+1, NewMLL, NewBody, More},
    Full = {next_state, full, NewState},
    case byte_size(NewMLL) + byte_size(NewBody) of
        X when X >= ?PARC_MAX_MEM -> Full;
        _ -> case (Count + 1) of
            ?PARC_MAX_MSG -> Full;
            _ -> concat(NewState)
        end
    end.

append({Count, MLL, Body, Msgs}, NewMsg) ->
    L = meeqo_msg:encode_length(byte_size(NewMsg)),
    NewMLL = <<MLL/binary, L/binary>>,
    NewBody = <<Body/binary, NewMsg/binary>>,
    NewState = {Count+1, NewMLL, NewBody, Msgs},
    Full = {next_state, full, NewState},
    case byte_size(NewMLL) + byte_size(NewBody) of
        X when X >= ?PARC_MAX_MEM -> Full;
        _ -> case (Count + 1) of
            ?PARC_MAX_MSG -> Full;
            _ -> {next_state, ready, NewState}
        end
    end.

