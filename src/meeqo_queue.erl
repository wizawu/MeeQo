%%
%%  Copyright (C) 2012 Hualiang Wu <wizawu@gmail.com>
%%
%%  This file is part of MeeQo.
%%
%%  MeeQo is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  MeeQo is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with MeeQo.  If not, see <http://www.gnu.org/licenses/>.
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

