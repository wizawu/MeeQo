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

-module(meeqo_session).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {ts, top}).


start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


init([]) ->
    % ts is the timestamp of the current process and top is the timestamp of
    % oldest unread message.
    {ok, #state{ts = 0, top = 1}}.


handle_call({save, Msg, Uts}, _From, State) ->
    #state{ts = Ts, top = Top} = State,
    put(Ts + 1, {Msg, Uts}),
    % Return the minimum unified timestamp to update the queueing order.
    {_, MinUts} = get(Top),
    {reply, MinUts, State#state{ts = Ts + 1}};

handle_call(read, _From, State) ->
    #state{top = Top} = State,
    % Keep in mind to free the memory.
    case erase(Top) of
        {Msg, _} ->
            MinUts = case get(Top + 1) of
                {_, Uts} -> Uts;
                undefined ->
                    % In this situation, there is no more message.
                    erlang:send_after(30000, self(), idle),
                    nil
            end,
            {reply, {Msg, MinUts}, State#state{top = Top + 1}};
        undefined ->
            {reply, nil, State}
    end;

handle_call(_Request, _From, State) ->
    {noreply, State}.


handle_info(idle, State) ->
    #state{top = Top} = State,
    case get(Top) of
        undefined -> {stop, 'IDLE', State};
        _ -> {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

