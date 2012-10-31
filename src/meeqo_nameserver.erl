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

-module(meeqo_nameserver).

-export([start_link/0]).

-include("./meeqo_config.hrl").

-define(GRP_TABLE, meeqo_nameserver_grp_table).
-define(REG_TABLE, meeqo_nameserver_reg_table).

-define(TCP_OPTION, [binary, {active,false}, {packet,0}, {reuseaddr,true}]).

start_link() ->
    case lists:member(?GRP_TABLE, ets:all()) of
        false -> ets:new(?GRP_TABLE, [bag, public, named_table]);
        true -> ok
    end,
    case lists:member(?REG_TABLE, ets:all()) of
        false -> ets:new(?REG_TABLE, [set, public, named_table]);
        true -> ok
    end,
    case gen_tcp:listen(?MEEQO_NAMESERVER_PORT, ?TCP_OPTION) of
        {ok, LSock} -> 
            spawn(fun() -> accept(LSock) end),
            Pid = spawn(fun() -> loop() end),
            true = register(?MODULE, Pid);
        {error, Reason} -> {error, Reason}
    end.
        
% handle pid request
loop() ->
    receive
        {Pid, Msg} when is_pid(Pid) ->
            spawn(fun() -> read({Pid, Msg}) end),
            loop();
        _ -> loop()
    end.

read({Pid, Msg})->
    case Msg of
        {886} -> remove(Pid);
        {GrpName} when is_atom(GrpName) -> resolve(GrpName, Pid);
        {GrpList} when is_list(GrpList) -> add(Pid, GrpList);
        _ -> error
    end.

% handle tcp request
accept(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> accept(LSock) end),
    recv(Sock).

recv(Sock) ->
    {ok, Client} = inet:peername(Sock),  % Client -> {Address, Port}
    {ok, Bin} = gen_tcp:recv(Sock, 0),
    T = binary_to_term(Bin),
    case T of
        {886} -> remove(Client);
        {GrpName} when is_atom(GrpName) -> resolve(GrpName, Sock);
        {GrpList} when is_list(GrpList) -> add(Client, GrpList);
        _ -> error
    end.


add(Client, GrpList) ->
    remove(Client),
    ets:insert(?REG_TABLE, {Client, GrpList}),
    Join = fun(Group) -> ets:insert(?GRP_TABLE, {Group, Client}) end,
    lists:map(Join, GrpList).

remove(Client) ->
    case ets:lookup(?REG_TABLE, Client) of
        [{Client, GrpList}] -> ets:lookup(?REG_TABLE, Client);
        _ -> GrpList = []
    end,
    Quit = fun(Group) -> ets:delete_object(?GRP_TABLE, {Group, Client}) end,
    lists:map(Quit, GrpList),
    ets:delete(?REG_TABLE, Client).

resolve(GrpName, Pid) when is_pid(Pid) ->
    List = ets:lookup(?GRP_TABLE, GrpName),
    Reply = [V || {_K, V} <- List],
    Pid ! Reply;
        
resolve(GrpName, Sock) ->
    List = ets:lookup(?GRP_TABLE, GrpName),
    Reply = term_to_binary({[V || {_K, V} <- List]}),
    gen_tcp:send(Sock, Reply).

