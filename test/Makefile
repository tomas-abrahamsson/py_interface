ERLC=erlc

ERLS = test_erl_node_pingpong.erl

BEAMS = $(patsubst %.erl,%.beam,$(ERLS))

all: $(BEAMS)

test: all
	./run_test_erl_node_pingpong.sh

qc quickcheck: all
	./run_test_erl_node_pingpong_qc.sh

%.beam: %.erl
	$(ERLC) -Wall $<
