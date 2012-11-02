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

-module(meeqo).

-export([start/0, setsocket/1, setcourier/1]).
-export([join/1, drop/0]).
-export([message/1, message/2]).
-export([send/2, send/3]).
-export([consign/1, consign/2]).
-export([check/0, check/1]).
-export([fetch/0, fetch/1]).
-export([discard/0, discard/1]).
-export([flush/0, flush/1]).

start() ->

setsocket(Opts) when is_list(Opts) ->

setcourier(random) ->

setcourier(Courier) when is_list(Courier) ->
    case io_lib:pritable_list(Courier) of
        true -> 
        false -> error
    end.

join(GrpList) when is_list(GrpList) ->

drop() ->

message(Msg) ->

message(Msg, Opt) ->

send(Who, Msg) ->

send(Who, Msg, now) ->
    send(Who, Msg, 0);

send(Who, Msg, Delay) when is_integer(Delay) ->

consign(Msg) ->

consign(Msg, now) ->
    consign(Msg, 0);

consign(Msg, Delay) when is_integer(Delay) ->

check() ->

check(From) ->

fetch() ->

fetch(Courier) ->

discard() ->

discard(Courier) ->

flush() ->

flush(From) ->

