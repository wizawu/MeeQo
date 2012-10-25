%%
%% Copyright (C) 2012 Hualiang Wu <wizawu@gmail.com>
%%
%% This file is part of MeeQo.
%%
%% MeeQo is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% MeeQo is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with MeeQo.  If not, see <http://www.gnu.org/licenses/>.
%%

-module(meeqo_address).

-export([resolve/1]).

resolve(Str) ->
    case Str of
        "tcp://" ++ Address -> {tcp, Address};
        "ipc://" ++ RegName -> {ipc, whereis(list_to_atom(RegName))};
        "grp://" ++ GrpName -> {grp, list_to_atom(GrpName)};
        _ when is_pid(Str)  -> {ipc, Str};
        _ -> error
    end.
