SOURCES = erl_node.py \
	  erl_node_conn.py \
	  erl_async_conn.py \
	  erl_opts.py \
	  erl_common.py \
	  erl_epmd.py \
	  erl_eventhandler.py \
	  erl_term.py




TESTPROGRAMS = test_erl_epmd.py \
	       test_erl_node.py \
	       test_erl_node_conn.py \
	       test_erl_node_tk_1.py

DISTFILES = $(SOURCES) $(TESTPROGRAMS) COPYING.LIB README Makefile

rm=rm
ln=ln
mkdir=mkdir
tar=tar

dist: $(DISTFILES)
	version=`perl -ne 'print $$1 if /py_interface_version ?= ?\"(.*)\"/' \
		 erl_node.py`; \
	distname=py_interface-$$version; \
	$(rm) -rf $$distname; \
	$(mkdir) $$distname; \
	for file in $(DISTFILES); do \
	  $(ln) $$file $$distname/$$file; \
	done; \
	$(tar) -chz -f $$distname.tar.gz $$distname; \
	$(rm) -rf $$distname
