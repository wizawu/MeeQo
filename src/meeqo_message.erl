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

new(Msg) ->
    Bin = term_to_binary({Msg}),
    Len = bit_size(Bin),
    <<M:Len>> = Bin,
    {ok, <<?MS_DEF_HEAD:8, M:Len>>}.

new(Msg, Opts) when is_tuple(Msg) ->
    
option(Opts) when is_list(Opts) ->
    option(<<?MS_DEF_HEAD:8>>, Opts).
    
option(X, []) -> X;
option(X, Opts) ->
    Y = case hd(Opts) of
        {deliver, false} -> X band (bnot ?MS_DLVR);
        {deliver, true}  -> X bor ?MS_DLVR; 
        {reserve, false} -> X band (bnot ?MS_RESV);
        {reserve, true}  -> X bor ?MS_RESV;
        {long, flase} -> X band (bnot ?MS_LONG);
        {long, true}  -> X bor ?MS_LONG;
        {segm, flase} -> X band (bnot ?MS_SEGM);
        {segm, true}  -> X bor ?MS_SEGM;
        {dest, false} -> X band (bnot ?MS_DEST);
        {dest, Addr}  -> X bor ?MS_DEST;
        {from, false} -> X band (bnot ?MS_FROM);
        {from, Addr}  -> X bor ?MS_FROM
    end,
    option(Y, tl(Opts)).

