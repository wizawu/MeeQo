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

-export([new/0, is_empty/1, len/1, front/1, push/2, pop/1]).

% O(1)
new() -> {[], [], 0}.

% O(1)
is_empty({[], [], 0}) -> true;
is_empty({_L, _R, _Len}) -> false.

% O(1)
len({_L, _R, Len}) -> Len.

% O(1)
front({L, _R, Len}) -> 
    case Len of
        0 -> nil;
        _ -> {ok, hd(L)}
    end.
    
% O(1) amortized
push({L, R, Len}, X) ->
    case L of
        [] -> {[X], [], 1};
        _ -> {L, [X|R], Len + 1}
    end.

% O(1) amortized
% must check is_empty/1 ahead
pop({L, R, Len}) ->
    case L of
        [_X] -> {lists:reverse(R), [], Len - 1};
        _ -> {tl(L), R, Len - 1}
    end.
