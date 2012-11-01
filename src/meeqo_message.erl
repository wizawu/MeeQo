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

-module(meeqo_message).

-export([new/1, new/2]).

-include("./meeqo_protocol.hrl").

new(Msg) when is_tuple(Msg) ->
    Bin = term_to_binary(Msg),
    Len = bit_size(Bin),
    <<M:Len>> = Bin,
    {ok, <<?MS_DEFAULT_HEAD:8, M:Len>>}.

new(Msg, Option) when is_tuple(Msg) ->
    
setopt(Msg, Option) 

head(Opts) when is_list(Opts) ->
    SrcAddr = lists:filter(fun src_addr/1, Opts),
    DestAddr = lists:filter(fun dest_addr/1, Opts),
    OptList = lists:map(fun opt_to_byte/1, Opts),
    Head = bytes_or(OptList),


opt_to_byte(X) ->
    case X of
        alloc -> ?MS_ALLOC;
        triv  -> ?MS_TRIV;
        long  -> ?MS_LONG;
        segm  -> ?MS_SEGM;
        {dest, Addr} -> ?MS_DEST;
        {src, Addr} -> ?MS_SRC;
        _ -> 2#00000000
    end.

bytes_or(OptBytes) when is_list(OptBytes) ->
    bytes_or(?MS_DAFAULT_HEAD, OptBytes).

bytes_or(Head, []) -> Head;
bytes_or(Head, OptBytes) ->
    [H|T] = OptBytes,
    bytes_or(Head bor H, T).


src_addr(Opt) ->
    if 
        is_tuple(Opt) ->
            case Opt of
                {src, Addr} -> true;
                _ -> false
            end;
        true -> false
    end.
    
dest_addr(Opt) ->
    if 
        is_tuple(Opt) ->
            case Opt of
                {dest, Addr} -> true;
                _ -> false
            end;
        true -> false
    end.

