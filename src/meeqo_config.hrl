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

% Default port. It MUST be an odd number. If you're going to run multiple MeeQo
% instances on the same machine, specify different ports for them.
-define(PORT, 6611).

% When you send messages to MeeQo proxy or receive messages from it, you'd
% better set your socket buffer to the value below. The defaults are 4 MiB for
% RCVBUF and 32 KiB for SNDBUF. It is strongly recommended to increase the 
% SNDBUF to MiB level. The system tuning refers 
% http://www.psc.edu/index.php/networking/641-tcp-tune.
-define(PROXY_RCVBUF, 4194304).
-define(PROXY_SNDBUF, 32768).

% The size of socket buffer that MeeQo use to send messages to other instances.
% The default is 4 MiB, which means both RCVBUF and SNDBUF are 4 MiB.
-define(PIPE_BUF, 4194304).

% The maximum number of sockets that single MeeQo instance can use to send
% messages to other MeeQo instances. The messages sent to the same destination
% are guaranteed FIFO.
-define(MAX_ACT_PIPES, 32).

% MeeQo will pack small pieces of messages into a larger parcel automatically.
% You can limit parcel's maximum size by specifying the following two values.
% The defaults are 4000000 bytes and 100000.
-define(PARCEL_MAX_MEM, 4000000).
-define(PARCEL_MAX_MSG, 100000).

