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

-export([start_link/1]).

-include("meeqo_config.hrl").

-define(SOCKOPT, [binary, {active, false}, {packet, 4},
                  {recbuf, ?PIPE_BUF},
                  {sndbuf, ?PIPE_BUF}
                 ]).

start_link([SysTbl, Port]) when is_integer(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, ?SOCKOPT),
    [{_, InboxPid}] = ets:lookup(SysTbl, meeqo_inbox),
    [{_, LockerTid}] = ets:lookup(SysTbl, meeqo_locker),
    process_flag(trap_exit, true),
    Args = [LSock, InboxPid, LockerTid],
    new_listener(Args),
    loop(Args).

new_listener(Args) ->
    Pid = spawn_link(fun() -> listen([self() | Args]) end),
    put(listener, Pid).

loop(Args) ->
    receive
        accepted -> new_listener(Args);
        {'EXIT', Pid, _Why} ->
            case get(listener) of
                Pid -> 
                    % If listener fails, warn and new another.
                    error_logger:error_msg("Sink listener ~w failed.~n", [Pid]),
                    timer:sleep(200),
                    new_listener(Args);
                _ -> ok
            end
    end,
    loop(Args).
    
listen([LoopPid, LSock, InboxPid, LockerTid]) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    % Inform the main loop to create a new listener.
    LoopPid ! accepted,
    recv(Sock, InboxPid, LockerTid).

recv(Sock, InboxPid, LockerTid) ->
    inet:setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, Parc} ->
            gen_tcp:send(Sock, <<"ok">>),
            {ok, {PeerIp, _}} = inet:peername(Sock),
            {PeerPort, MsgList} = meeqo_msg:unpack(Parc),
            Save = fun(Msg) -> 
                % Use a reference to represent the message in order to prevent
                % copying messages across processes.
                Ref = make_ref(),
                ets:insert(LockerTid, {Ref, Msg}),
                gen_server:cast(InboxPid, {save, {PeerIp, PeerPort}, Ref})
            end,
            lists:map(Save, MsgList),
            recv(Sock, InboxPid, LockerTid);
        {tcp_closed, Sock} -> ok
    end.

