#! /bin/sh

# A test case for testing packing/unpacking of erlang-terms:
#
# A message is sent from an erlang node to a python node.
# That message is echoed back to the erlang node, which checks
# if the received message matches the original message.
#

erl=${ERL:-erl}

# First make sure epmd is up and running. (needed by the python-node)
$erl -noinput -detach -sname ensure_epmd_started@localhost -s erlang halt

pylogfile=test_erl_node_pingpong.log-py

# Now start the pythonnode
PYTHONPATH=..:$PYTHONPATH ./test_erl_node_pingpong.py \
    -d -n py_interface_test@localhost -c cookie \
         > $pylogfile 2>&1 &
pynode=$!

$erl +B -noinput -sname enode1@localhost \
    -setcookie cookie \
    -s test_erl_node_pingpong start_halt

ec=$?

/bin/kill $pynode 2>/dev/null

if [ $ec != 0 ]
then
    echo "==The Python node log file====================================="
    cat "$pylogfile"
    echo "==============================================================="
    exit 1
fi
