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

-ifndef(MEEQO_CONFIG).
-define(MEEQO_CONFIG, defined).

% nameserver 
% tcp(without port), pid or ipc address
-define(MEEQO_NAMESERVERS, ["tcp://localhost"]).
-define(MEEQO_NAMESERVER_PORT, 7179).

% courier
% tcp(without port), pid or ipc address
-define(MEEQO_COURIERS, ["tcp://localhost"]).
-define(MEEQO_COURIER_PORT, 7178).

% client
-define(MEEQO_CLIENT_PORT, 7177).

% parcel
-define(PARCEL_MAX_ITEMS, 1000).
-define(PARCEL_MAX_SIZE, 16#1000).  % bytes
-define(PARCEL_MAX_DELAY, 1000).    % ms

-endif.

