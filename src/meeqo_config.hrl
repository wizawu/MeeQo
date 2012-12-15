%%
%%  Copyright (c) 2012 Hualiang Wu <wizawu@gmail.com>
%%
%%  Permission is hereby granted, free of charge, to any person obtaining a copy
%%  of this software and associated documentation files (the "Software"), to
%%  deal in the Software without restriction, including without limitation the
%%  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%%  sell copies of the Software, and to permit persons to whom the Software is
%%  furnished to do so, subject to the following conditions:
%%
%%  The above copyright notice and this permission notice shall be included in
%%  all copies or substantial portions of the Software.
%%
%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%%  IN THE SOFTWARE.
%%

-ifndef(MEEQO_CONFIG).
-define(MEEQO_CONFIG, ok).

% The maximum number of clients that single MeeQo instance can handle
% simultaneously.
-define(MAX_CLIENTS, 32768).

% The maximum number of parallel message channels that single MeeQo instances
% can handle simultaneously. It won't guarantee the arrving order of messages 
% which have different destinations when using multiple message channels.
-define(MAX_CHANNELS, 32).

% When you send messages to MeeQo proxy or receive messages from it, you'd
% better set your user-level software buffer used by the driver to the value
% below. It is nightmare when it is too small. The default is 4194304 bytes.
-define(PROXY_BUFFER, 4194304).

% MeeQo will pack small pieces of messages into a larger parcel automatically.
% You can limit parcel's maximum size by specifying the following two values.
% Both of the defaults are 4000000.
-define(PARCEL_MAX_MEM, 4000000).
-define(PARCEL_MAX_MSG, 4000000).

% You can NEVER change these table names.
-define(LOCKER, meeqo_locker).
-define(INBOX, meeqo_inbox).
-define(OUTBOX, meeqo_outbox).

-endif.
