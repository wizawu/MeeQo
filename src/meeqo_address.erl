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
%%  API
%%-----------------------------------------------------------------------------
resolve(Addr) when is_list(Addr) ->
    case Addr of
        "tcp://" ++ HP -> Port = re:replace(HP, "\\S+:", "", [{return,list}]),
                          Host = re:replace(HP, ":"++Port++"$", "", [{return,list}]),
                          X = (HP == Host ++ ":" ++ Port),
                          case X of
                              true -> getaddr(Host, list_to_integer(Port));
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
    end;

resolve(_) -> error.

%%-----------------------------------------------------------------------------
encode({tcp, IP, Port}) ->
    case size(IP) of
        4 -> {A,B,C,D} = IP,
             <<?AD_IPV4, A:8, B:8, C:8, D:8, Port:16>>;
        8 -> L = 16,
             {A,B,C,D,E,F,G,H} = IP,
             <<?AD_IPV6, A:L, B:L, C:L, D:L, E:L, F:L, G:L, H:L, Port:16>>;
        _ -> error
    end;

encode({pid, Pid}) ->
    S = pid_to_list(Pid) -- "<>",
    R = re:split(S, "[.]", [{return, list}]),
    L = lists:map(fun erlang:list_to_integer/1, R),
    [A,B,C] = L,
    <<?AD_PID, A:32, B:32, C:32>>;

encode({grp, GrpName}) ->
    S = atom_to_list(GrpName),
    Len = length(S),
    if 
        Len < 2#11110000 ->
            list_to_binary([Len|S]);
        true ->
            error
    end;

encode(Addr) when is_list(Addr) ->
    case resolve(Addr) of
        error -> error;
        X -> encode(X)
    end;

encode(_) -> error.

%%-----------------------------------------------------------------------------
decode(Bin) when is_binary(Bin) ->
    L = bit_size(Bin) - 8,
    if
        L > 0 ->
            <<H:8, T:L>> = Bin,
            decode(H, <<T:L>>);
        true ->
            error
    end;

decode(_) -> error.

%%-----------------------------------------------------------------------------
%%  internal functions
%%-----------------------------------------------------------------------------
getaddr(Host, Port) ->
    case inet_parse:address(Host) of
        {ok, IP} ->
            {tcp, IP, Port};
        _ ->
            error
    end.

%%-----------------------------------------------------------------------------
decode(?AD_IPV4, Bin) ->
    <<A:8, B:8, C:8, D:8, P:16>> = Bin,
    {tcp, {A,B,C,D}, P};

decode(?AD_IPV6, Bin) ->
    <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16, P:16>> = Bin,
    {tcp, {A,B,C,D,E,F,G,H}, P};

decode(?AD_PID, Bin) ->
    <<A:32, B:32, C:32>> = Bin,
    S = "<" ++ integer_to_list(A) ++ "." ++ integer_to_list(B)
        ++ "." ++ integer_to_list(C) ++ ">",
    {pid, list_to_pid(S)};

decode(AddrHead, Bin) when AddrHead < 2#11110000 ->
    {grp, list_to_atom(binary_to_list(Bin))};

decode(_, _) -> error.

