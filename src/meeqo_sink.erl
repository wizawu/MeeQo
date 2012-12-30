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

-module(meeqo_sink).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {listen, lsock, systbl}).

-include("meeqo_config.hrl").

-define(SOCKOPT, [binary, {active, false}, {packet, 4},
                  {recbuf, ?PIPE_BUF},
                  {sndbuf, ?PIPE_BUF}
                 ]).

start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).

init([SysTbl]) ->
    [Port] = meeqo:info(SysTbl, [port]),
    % The port of meeqo_sink is the port of meeqo_proxy plus one.
    {ok, LSock} = gen_tcp:listen(Port+1, ?SOCKOPT),
    ets:insert(SysTbl, {?MODULE, self()}),
    process_flag(trap_exit, true),
    Listen = spawn_link(fun() -> listen(self(), LSock, SysTbl) end),
    {ok, {Listen, LSock, SysTbl}}.

handle_cast(accepted, State) ->
    {_, LSock, SysTbl} = State,
    Listen = spawn_link(fun() -> listen(self(), LSock, SysTbl) end),
    {noreply, State#state{listen = Listen}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Why}, State) ->
    {_, LSock, SysTbl} = State,
    NewState = case State#state.listen of
        Pid ->
            % If listener fails, warn and new another.
            Format = "meeqo_sink listener ~w failed.",
            error_logger:error_msg(Format, [Pid]),
            timer:sleep(1000),
            Listen = spawn_link(fun() -> listen(self(), LSock, SysTbl) end),
            State#state{listen = Listen};
        _ -> State
    end,
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

listen(Svr, LSock, SysTbl) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    % Inform the server to create a new listener.
    gen_server:cast(Svr, accepted),
    [Inbox] = meeqo:info(SysTbl, [meeqo_inbox]),
    recv(Sock, Inbox).

recv(Sock, Inbox) ->
    inet:setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, Parc} ->
            gen_tcp:send(Sock, <<"ok">>),
            {ok, {PeerIp, _}} = inet:peername(Sock),
            {PeerPort, MsgList} = meeqo_msg:unpack(Parc),
            Save = fun(Msg) -> 
                % Use a reference to represent the message in order to avoid
                % copying messages across processes.
                Ref = make_ref(),
                ets:insert(meeqo_locker, {Ref, Msg}),
                gen_server:cast(Inbox, {save, {PeerIp, PeerPort}, Ref})
            end,
            lists:map(Save, MsgList),
            recv(Sock, Inbox);
        {tcp_closed, Sock} -> ok
    end.

