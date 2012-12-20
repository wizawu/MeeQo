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

-record(state, {mll, body, msgs}).

-include("meeqo_config.hrl").

-define(SOCKOPT, [{binary, {active, false}, {packet, 4},
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
    spawn_link(fun() -> fitter(self()) end),
    {ok, empty, #state{<<>>, <<>>, []}}.

empty(concat, State) ->
    #state{mll = MLL, body = Body} = State,


state_name(_Event, State) ->
    {next_state, state_name, State}.

handle_event({send, Msgs}, StateName, State
handle_event(send, _StateName, State) ->
    #state{mll = MLL, body = Body, msgs = Msgs} = State,
    ProxyPort = get(proxyPort),
    MLLBytes = byte_size(MLL),
    Parc = <<ProxyPort:16, MLLBytes:32, MLL/binary, Body/binary>>,
    Sock = get(sock),
    gen_tcp:send(Sock, Parc),
    {ok, <<"ok">>} = gen_tcp:recv(Sock, 0),
    Sluice = get(sluice),
    case Msgs of
        [] -> gen_server:cast(Sluice, sent),
        _ -> gen_server:cast(Sluice, {sent, self()})
    end,
    {next_state, empty, State#state{mml = <<>>, body = <<>>}};
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

fitter(Pid) ->
    T = gen_fsm:sync_send_event(Pid, concat),
    timer:sleep(T),
    fitter(Pid).

