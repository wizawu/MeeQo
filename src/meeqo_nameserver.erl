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

-behavior(gen_server).

-export([start_link/0, register/2, unregister/1]).

-export([init/1, handle_call/3,  handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {grp_table, reg_info}).

-include("./meeqo_config.hrl").

-define(GRP_TABLE, meeqo_nameserver_grp_table).
-define(REG_TABLE, meeqo_nameserver_reg_table).

%%-----------------------------------------------------------------------------
%%  API
%%-----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(Address, []) -> ok.

register(Address, Groups) ->
    [H|T] = Groups,
    add(Address, H),
    register(Address, T).

unregister(Address) ->

resolve(GrpName) ->

%%-----------------------------------------------------------------------------
%%  callback
%%-----------------------------------------------------------------------------
init([Port]) ->
    {ok, LSocket} = gen_tcp:listen(Port, []),
    GrpTable = ets:new(?GRP_TABLE, [bag, protected, named_table],
    RegTable = ets:new(?REG_TABLE, [set, protected, named_table],
    {ok, 
    

handle_call(resolve, _From, State) ->
    {reply, {ok, 

handle_cast(register, _From) ->

handle_cast(unregister, _From) ->

handle_info({tcp, Socket, BinData}, State) ->


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%%  internal function
%%-----------------------------------------------------------------------------
add({Member, GrpList}) ->
    remove(Member),
    ets:insert(?REG_TABLE, {Member, GrpList}),
    Join = fun(Group) -> ets:insert(?GRP_TABLE, {Group, Member}) end,
    lists:map(Join, GrpList).

remove(Member) ->
    GrpList = ets:lookup(?REG_TABLE, Member),
    Quit = fun(Group) -> ets:delete_object(?GRP_TABLE, {Group, Member}) end,
    lists:map(Quit, GrpList),
    ets:delete(?REG_TABLE, Member).

resolve(Group) ->
    List = ets:lookup(?GRP_TABLE, Group),
    [V || {K, V} <- List].




