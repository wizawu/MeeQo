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

-module(meeqo).

-behaviour(supervisor).

-export([start_link/0, start_link/1]).

-export([init/1]).

-include("meeqo_config.hrl").

-define(PROXY_SOCKOPT, [binary, {active, false}, 
                        {recbuf, ?PROXY_RCVBUF}, 
                        {sndbuf, ?PROXY_SNDBUF}
                       ]).

-define(PIPE_SOCKOPT, [binary, {active, false},
                       {recbuf, ?PIPE_BUF},
                       {sndbuf, ?PIPE_BUF}
                      ]).

start_link() -> start_link(?PORT).

start_link(Port) when is_integer(Port) ->
    case (Port band 1) of
        0 -> exit('The port number MUST be odd');
        1 -> supervisor:start_link(?MODULE, [Port])
    end. 

init([Port]) ->
    Name = "meeqo_" ++ integer_to_list(Port),
    ets:new(list_to_atom(Name), [set, public, named_table]),
    Strategy = {one_for_all, 0, 1},
    Router = {router, {meeqo_router, start_link, [Port+1]},
              brutal_kill, worker,[meeqo_router]},
    Inbox  = {inbox, {meeqo_inbox, start_link, []},
              brutal_kill, worker,[]},
    Proxy  = {proxy, {meeqo_proxy, start_link, [Port]},
              brutal_kill, worker,[]},
    {ok, {Strategy, [Router, Inbox, Proxy]}}.





