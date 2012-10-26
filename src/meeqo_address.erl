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

-export([resolve/1]).

resolve(Str) ->
    case Str of
        "tcp://" ++ IPnP -> Port = re:replace(IPnP, "\\S+:", "", [{return,list}]),
                            IP = re:replace(IPnP, ":"++Port++"$", "", [{return,list}]),
                            Bool = (IPnP == IP ++ ":" ++ Port),
                            case Bool of
                                true -> {tcp, IP, Port};
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

encode(AddrTerm) ->

decode(AddrBin) ->
