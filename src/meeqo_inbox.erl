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

-module(meeqo_inbox).

-export([new/0]).

new() ->
    put(q, meeqo_queue:new()),
    put(n, 0),  % the number of skip-read messages
    loop().

loop() ->
    N = get(n),
    receive
        {read, Pid} ->
            if  N > 0 -> put(n, N - 1),
                         Pid ! pass;
                N == 0 -> Pid ! pop()
            end;
        {sread, Pid} ->
            X = pop(),
            if  X == nil -> pass;
                true -> put(n, N + 1)
            end,
            Pid ! X;
        _ -> pass
    end,
    loop().

pop() ->
    Q = get(q),
    X = meeqo_queue:front(Q),
    case X of
        {ok, _} -> put(q, meeqo_queue:pop(Q)), X;
        nil -> nil
    end.
