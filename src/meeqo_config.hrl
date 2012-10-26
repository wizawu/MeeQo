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

-ifndef(MEEQO_CONFIG).
-define(MEEQO_CONFIG).

%%-------------------------------------
%% meeqo_nameserver 
%%-------------------------------------
-define(MEEQO_NAMESERVER, []).
-define(MEEQO_NAMESERVER_PORT, 7179).

%%-------------------------------------
%% meeqo_courier
%%-------------------------------------
-define(MEEQO_COURIER, []).
-define(MEEQO_COURIER_PORT, 7178).

%%-------------------------------------
%% other
%%-------------------------------------
-define( ,2#1000000).
-define( ,2#0100000).
-define( ,2#0010000).
-define( ,2#0001000).
-define( ,2#0000100).
-define( ,2#0000010).
-define( ,2#0000001).

-define(NOAD, 2#0000).
-define(PID,  2#0001).
-define(IPC,  2#0010).
-define(IPV4, 2#0100).
-define(IPV6, 2#0110).
-define(GRP,  2#1111).


-endif.

