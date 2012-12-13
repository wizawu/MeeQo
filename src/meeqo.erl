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

-module(meeqo).

-export([start/0, start/1, close/0]).
-export([regist/1, unregist/1, resolve/1]).
-export([msg/1, msg/2]).
-export([send/2, send/3]).
-export([post/2]).
-export([read/0, read/1]).
-export([fetch/0, fetch/1]).

-include("./meeqo_config.hrl").

%%-----------------------------------------------------------------------------
%%  API
%%-----------------------------------------------------------------------------
start() ->
    start(?MEEQO_CLIENT_PORT).

start(Port) ->
    case gen_tcp:listen(Port) of
        {ok, LSock} -> gen_server:stark_link(meeqo_socket, [LSock], []);
        _ -> error
    end.

close(MQSockRef) ->
    unregist(MQSockRef, all),
    exit(MQSockRef, 'EXIT').

regist(GrpList) when is_list(GrpList) ->


msg(Msg) ->
    meeqo_message:new(Msg).

msg(Msg, Opts) when is_list(Opts) ->
    meeqo_message:new(Msg, Opts).

send(MQSockRef, Msg, Who) when is_list(Who) ->
    ok.

post(MQSockRef, Msg) -> 
    ok.

read() ->
    ok.

read(From) ->
    ok.

fetch() ->
    ok.

fetch(PostAmt) when is_list(Postamt) ->
    ok.

