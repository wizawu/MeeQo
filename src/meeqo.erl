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

-export([start/0, start/1, close/0]).
-export([regist/1, syncns/0, syncns/1]).
-export([setsocket/1, setcourier/1]).
-export([msg/1, msg/2]).
-export([send/2, send/3]).
-export([consign/1, consign/2]).
-export([check/0, check/1]).
-export([fetch/0, fetch/1]).
-export([empty/0, empty/1]).
-export([flush/0, flush/1]).

-include("./meeqo_config.hrl").

start() ->
    start(?MEEQO_CLIENT_PORT).

start(Port) ->
    case gen_tcp:listen(Port) of
        {ok, LSock} -> ;
        _ -> error
    end.

close() ->
    exit(self(), 'EXIT').

regist(GrpList) when is_list(GrpList) ->

syncns() ->

syncns(Grp) when is_atom(Grp) ->

setsocket(Opts) when is_list(Opts) ->

setcourier(random) ->

setcourier(Courier) when is_list(Courier) ->

message(Msg) ->
    meeqo_message:new(Msg).

message(Msg, Opts) when is_list(Opts) ->
    meeqo_message:new(Msg, Opts).

% do not include dest in Msg-Opts when using send
send(Who, Msg) when is_list(Who) ->

send(Who, Msg, now) when is_list(Who) ->
    send(Who, Msg, 0);

send(Who, Msg, Delay) when is_integer(Delay) ->

% 
consign(Msg) ->

consign(Msg, now) ->
    consign(Msg, 0);

consign(Msg, Delay) when is_integer(Delay) ->

check() ->

check(From) when is_list(From) ->

fetch() ->

fetch(Courier) when is_list(Courier) ->

empty() ->

empty(From) when is_list(From) ->

flush() ->

flush(Courier) when is_list(Courier) ->

