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

% message: 1 * 32bit + Bin
% parcel:  first 4 byte int describe k
%          decode from byte 5 to k+4 to get a list
%          each element in list represent a msg len

-module(meeqo_message).

-export([new/1, new/2]).

-include("./meeqo_protocol.hrl").

new(Msg) ->
    Bin = term_to_binary({Msg}),
    L = bit_size(Bin),
    <<X:L>> = Bin,
    <<?MS_DEF_HEAD:8, X:L>>.

new(Msg, Opts) when is_list(Opts) ->
    H = option(Opts),
    {D, DL} = dest(Opts),
    {F, FL} = from(Opts), 
    Bin = term_to_binary({Msg}),
    L = bit_size(Bin),
    <<X:L>> = Bin,
    <<H:8, D:DL, F:FL, X:L>>. 
    
option(Opts) ->
    option(?MS_DEF_HEAD, Opts).
    
option(X, []) -> X;
option(X, Opts) ->
    Y = case hd(Opts) of
        {deliver, false} -> X band (bnot ?MS_DLVR);
        {reserve, false} -> X band (bnot ?MS_RESV);
        {long,    flase} -> X band (bnot ?MS_LONG);
        {segm,    flase} -> X band (bnot ?MS_SEGM);
        {dest,    false} -> X band (bnot ?MS_DEST);
        {from,    false} -> X band (bnot ?MS_FROM);
        {deliver,  true} -> X bor ?MS_DLVR; 
        {reserve,  true} -> X bor ?MS_RESV;
        {long,     true} -> X bor ?MS_LONG;
        {segm,     true} -> X bor ?MS_SEGM;
        {dest,    _Addr} -> X bor ?MS_DEST;
        {from,    _Addr} -> X bor ?MS_FROM
    end,
    option(Y, tl(Opts)).

dest([]) -> {0, 0};
dest(Opts) ->
    case hd(Opts) of
        {dest, Addr} -> 
            Bin = meeqo_address:encode(Addr),
            L = bit_size(Bin),
            <<X:L>> = Bin,
            {X, L};
        _ -> dest(tl(Opts))
    end.

from([]) -> {0, 0};
from(Opts) ->
    case hd(Opts) of
        {from, Addr} -> 
            Bin = meeqo_address:encode(Addr),
            L = bit_size(Bin),
            <<X:L>> = Bin,
            {X, L};
        _ -> from(tl(Opts))
    end.

