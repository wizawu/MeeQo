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

-module(meeqo_queue).

-export([new/0, empty/1, len/1, front/1, push/2, pop/1]).

% O(1)
new() -> {[], []}.

% O(1)
empty({[], []}) -> true;
empty({L, R}) when is_list(L), is_list(R) -> false.

% O(N)
len({L, R}) when is_list(L), is_list(R) ->
    length(L) + length(R).

% O(1)
front({[X|L], R}) when is_list(L), is_list(R) -> X.
    
% O(1) amortized
push({L, R}, X) when is_list(L), is_list(R) ->
    case L of
        [] -> {[X], []};
        _ -> {L, [X|R]}
    end.

% O(1) amortized
pop({[_X], R}) when is_list(R) ->
    {lists:reverse(R), []};
pop({[_X|L], R}) when is_list(L), is_list(R) ->
    {L, R}.

