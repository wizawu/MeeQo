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

-module(meeqo_msg).

-export([unpack/1]).

unpack(Parc) when is_binary(Parc) ->
    % The first four bytes indicate the length of the message-length "list",
    % which is in specific binary form and will be decoded later.
    <<HdByteSz:32, _/binary>> = Parc,
    HdBitSz = HdByteSz * 8,
    <<_:32, Head:HdBitSz/bitstring, Msgs/binary>> = Parc,
    % Get the integer list representing the lengths of each message in the
    % parcal sequentially.
    LenList = decode(Head),
    split(LenList, Msgs).


split(LenList, Msgs) -> split([], LenList, Msgs).

split(MsgList, [], <<>>) -> lists:reverse(MsgList);
split(SoFar, [H0|T], MsgBin) ->
    H = H0 * 8,
    <<M:H/bitstring, R/binary>> = MsgBin,
    split([M|SoFar], T, R).


decode(Head) -> decode([], Head).

decode(LenList, <<>>) -> lists:reverse(LenList);
decode(SoFar, <<L:8, R/binary>>) ->
    % If the message length is smaller than 255, use one byte to represent it.
    % Otherwise, use 255 followed by 5 bytes which represent the length.
    % Therefore the maximum message size is 1 TB.
    if L == 255 ->
        <<K:40>> = binary:part(R, {0, 5}),
        decode([K|SoFar], binary:part(R, {5, byte_size(R)-5}));
    true ->
        decode([L|SoFar], R)
    end.

