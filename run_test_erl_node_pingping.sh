#! /bin/sh

# A test case for testing packing/unpacking of erlang-terms:
#
# A message is sent from an erlang node to a python node.
# That message is echoed back to the erlang node, which checks
# if the received message matches the original message.
#

# First make sure epmd is up and running. (needed by the python-node)
erl -noinput -detach -sname ensure_epmd_started@localhost -s erlang halt

# Now start the pythonnode
./test_erl_node_pingpong.py -n py_interface_test@localhost -c cookie \
	 > test_erl_node_pingpong.log-py 2>&1 &
pynode=$!

erl -noinput -sname enode1@localhost \
    -setcookie cookie \
    -s test_erl_node_pingpong start \
    -s erlang halt

kill $pynode
