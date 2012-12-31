
-module(dbg_tracer).

-export([init/0]).

init() ->
    dbg:stop_clear(),
    dbg:tracer(),
    dbg:p(new, c),
    dbg:tpl(meeqo, '_', []),
    dbg:tpl(meeqo_inbox, '_', []),
    dbg:tpl(meeqo_sink, '_', []),
    dbg:tpl(meeqo_sluice, '_', []),
    dbg:tpl(meeqo_proxy, '_', []),
    dbg:tpl(meeqo_session, '_', []),
    dbg:tpl(meeqo_pipe, '_', []),
    dbg:tpl(meeqo_msg, '_', []),
    dbg:tp(gen_server, '_', []),
    dbg:tp(gen_fsm, '_', []),
    dbg:tp(gen_tcp, '_', []),
    dbg:tp(inet, '_', []),
    dbg:tp(erlang, put, []),
    dbg:tp(erlang, erase, []),
    dbg:tp(ets, insert, []),
    dbg:tp(ets, delete, []),
    dbg:tp(ets, new, []),
    ok.

