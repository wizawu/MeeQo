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

-ifndef(MEEQO_PROTOCOL).
-define(MEEQO_PROTOCOL, defined).

% message head options
-define(MS_PACK, 2#10000000).
-define(MS_DLVR, 2#01000000).
-define(MS_RESV, 2#00100000).
-define(MS_LONG, 2#00010000).
-define(MS_SEGM, 2#00001000).
-define(MS_DEST, 2#00000010).
-define(MS_FROM, 2#00000001).

-define(MS_DEFAULT_HEAD, 2#01000000).

% address head options
-define(AD_PID,  2#11110001).
-define(AD_IPV4, 2#11110010).
-define(AD_IPV6, 2#11110011).

-endif.

