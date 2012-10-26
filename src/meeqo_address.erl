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

-module(meeqo_address).

-export([resolve/1, encode/1, decode/1]).

-include("./meeqo_protocol.hrl").

%%-----------------------------------------------------------------------------

resolve(Addr) when is_list(Addr) ->
    case Addr of
        "tcp://" ++ HP -> Port = re:replace(HP, "\\S+:", "", [{return,list}]),
                          Host = re:replace(HP, ":"++Port++"$", "", [{return,list}]),
                          X = (HP == IP ++ ":" ++ Port),
                          case X of
                              true -> getaddr(Host, Port);
                              false -> error
                          end;
        "pid://" ++ Pid -> {pid, list_to_pid("<" ++ Pid ++ ">")};
        "ipc://" ++ RegName -> Pid = whereis(list_to_atom(RegName)),
                               if
                                   Pid == undefined ->
                                       error;
                                   true ->
                                       {pid, Pid}
                               end;
        "grp://" ++ GrpName -> {grp, list_to_atom(GrpName)};
        _ -> error
    end.


encode(Addr) when is_tuple(Addr) ->
    case Addr of
        {tcp, IP, Port} ->


decode(Addr) when is_binary(Addr) ->


%%-----------------------------------------------------------------------------

getaddr(Host, Port) ->
    case inet_parse:address(Host) of
        {ok, IP} ->
            {tcp, IP, Port};
        _ ->
            error
    end.

