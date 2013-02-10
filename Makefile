.PHONY: all move clean

beam = meeqo.beam meeqo_inbox.beam meeqo_msg.beam \
	   meeqo_pipe.beam meeqo_proxy.beam meeqo_session.beam \
	   meeqo_sink.beam meeqo_sluice.beam

all: $(beam) move

$(beam): %.beam: src/%.erl src/meeqo_config.hrl
	erlc -smp +native -o $@ $<

move:
	-cp $(beam) ebin/; rm $(beam)

clean:
	-cd ebin && rm $(beam)
