#! /bin/bash

# script debugging
#set -xv

# A test case for testing packing/unpacking of erlang-terms:
#
# A message is sent from an erlang node to a python node.
# That message is echoed back to the erlang node, which checks
# if the received message matches the original message.
#

erl=${ERL:-erl}

echo "Making sure we have epmd up and running..."
$erl +B -noinput -sname ensure_epmd_started@localhost -s erlang halt

echo "Starting the py_interface python node..."
pynodename=py_interface_test_qc@localhost
log=test_erl_node_pingpong_qc.log-py
/bin/rm -f "$log"
touch "$log"
env PYTHONPATH=..:"$PYTHONPATH" PYTHONUNBUFFERED=x \
    ./test_erl_node_pingpong.py -q -n "$pynodename" -c cookie > "$log" 2>&1 &
echo $! > pynode_pid
#tail -f "$log" &
#dedicated_follower=$!

echo "Starting the erlang qc node..."
$erl +B -noinput -sname qcnode1@localhost \
    -setcookie cookie \
    -s test_erl_node_pingpong_qc start_halt "$pynodename" "PYTHONPATH=..:'$PYTHONPATH' PYTHONUNBUFFERED=x ./test_erl_node_pingpong.py -q -n '$pynodename' -c cookie >> '$log' 2>&1"

ec=$?
[ -f pynode_pid ] && /bin/kill `cat pynode_pid` 2>/dev/null
/bin/rm -f pynode_pid
#kill $dedicated_follower

if [ $ec != 0 ]
then
    echo "==The Python node log file====================================="
    cat "$log"
    echo "==============================================================="
    exit 1
fi
