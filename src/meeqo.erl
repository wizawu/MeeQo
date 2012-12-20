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

start_link() -> start_link(?PORT).

start_link(Port) when is_integer(Port) ->
    case (Port band 1) of
        0 -> exit('The port number MUST be odd.');
        1 -> supervisor:start_link(?MODULE, [Port])
    end. 

init([Port]) ->
    % Create a table to record important process ids and table ids.
    SysTbl = list_to_atom("meeqo_" ++ integer_to_list(Port)),
    ets:new(SysTbl, [set, public, named_table]),
    % Locker is used to record references of messages.
    Locker = ets:new(anonym, [set, public]),
    ets:insert(SysTbl, {meeqo_locker, Locker}),
    Strategy = {one_for_all, 0, 1},
    Inbox  = {inbox, 
              {meeqo_inbox, start_link, [SysTbl]},
              temporary, brutal_kill, worker, 
              [meeqo_inbox]},
    Sink   = {sink, 
              {meeqo_sink, start_link, [SysTbl, Port + 1]},
              temporary, brutal_kill, worker,
              dynamic},
    Sluice = {sluice,
              {meeqo_sluice, start_link, [SysTbl]},
              temporary, brutal_kill, worker, 
              [meeqo_sluice]},
    Proxy  = {proxy,
              {meeqo_proxy, start_link, [SysTbl, Port]},
              temporary, brutal_kill, worker,
              dynamic},
    {ok, {Strategy, [Inbox, Sink, Sluice, Proxy]}}.

