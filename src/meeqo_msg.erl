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

-export([unpack/1, split/2, decode/1]).

% unpack(binary) -> {addr(), [binary, ...]}
% addr() -> {integer, ...}
% size(addr()) -> 5 | 9

unpack(Parc) when is_binary(Parc) ->
    % The first 2 bits indicate the ip address type. 00 for ipv4, 01 for ipv6.
    % The next 30 bits indicate the length of the message-length "list", which
    % is in specific binary form and will be decoded later. The following 4 or
    % 16 bytes indicate the ip address and 2 bytes indicate the port. All above 
    % make up the message header.
    <<AddrType:2/integer, MLLBytes:30/integer, _/binary>> = Parc,
    Addr = case AddrType of
        0 -> list_to_tuple([X || <<X>> <= binary_part(Parc, {4, 4})]
             ++ [X || <<X:16>> <= binary_part(Parc, {8, 2})]);
        1 -> list_to_tuple([X || <<X:16>> <= binary_part(Parc, {4, 16})]
             ++ [X || <<X:16>> <= binary_part(Parc, {20, 2})])
    end,
    % The header is followed by message-length list(MLL) and the main body.
    HdBits = 32 + (case AddrType of 0 -> 32; 1 -> 128 end) + 16,
    MLLBits = MLLBytes * 8,
    <<_:HdBits, MLL:MLLBits/bitstring, Msgs/binary>> = Parc,
    % Transform the MLL from binary to integer list and split the main body into 
    % messages according to the list.
    LenList = decode(MLL),
    {Addr, split(LenList, Msgs)}.

split(LenList, Msgs) -> split([], LenList, Msgs).

split(MsgList, [], <<>>) -> lists:reverse(MsgList);
split(SoFar, [H0|T], MsgBin) ->
    H = H0 * 8,
    <<M:H/bitstring, R/binary>> = MsgBin,
    split([M|SoFar], T, R).


decode(MLL) -> decode([], MLL).

decode(LenList, <<>>) -> lists:reverse(LenList);
decode(SoFar, <<L:8, R/binary>>) ->
    % If the message length is smaller than 255, use one byte to represent it.
    % Otherwise, use 255 followed by 5 bytes which represent the length.
    % Therefore the maximum message size is 1 TiB.
    if L == 255 ->
        <<K:40>> = binary:part(R, {0, 5}),
        decode([K|SoFar], binary:part(R, {5, byte_size(R)-5}));
    true ->
        decode([L|SoFar], R)
    end.

