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
erlc=${ERLC:-erlc}
eqc_path=${EQC_EBIN_PATH:-}

if [ x"$eqc_path" != x ]
then
    eqc_opt="-pa '$eqc_path'"
fi

# Verify that we have quickcheck(mini)
have_eqc="$(erl +B -boot start_clean -noinput -noshell $eqc_opt -eval \
                'case code:which(eqc) of
                      non_existing      -> io:format("no~n");
                      P when is_list(P) -> io:format("yes~n")
                 end.' -s erlang halt)"

if [ x"$have_eqc" != xyes ]
then
    echo "ERROR: QuickCheck (mini) needed but not found! Aborting." >&2
    echo "Download it from http://www.quviq.com/downloads/eqcmini.zip"
    echo "and set the ERL_LIBS environment variable to include it,"
    echo "or set the EQC_EBIN_PATH environment variable."
    echo
    echo "Example (zsh/bash/...):"
    echo "   export ERL_LIBS=/usr/local/eqcmini/eqc-1.0.1"
    echo "   export EQC_EBIN_PATH=/usr/local/eqcmini/eqc-1.0.1/ebin"
    echo
    echo "Example (csh/tcsh):"
    echo "   setenv ERL_LIBS /usr/local/eqcmini/eqc-1.0.1"
    echo "   setenv EQC_EBIN_PATH /usr/local/eqcmini/eqc-1.0.1/ebin"
    exit 1
fi

die () { echo "$@"; exit 1; }

if [ ! -f test_erl_node_pingpong_qc.beam -o \
    test_erl_node_pingpong_qc.erl -nt test_erl_node_pingpong_qc.beam ]
then
    echo "Compiling test_erl_node_pingpong_qc.erl..."
    $erlc -Wall $eqc_opt test_erl_node_pingpong_qc.erl \
	|| die "Failed"
fi

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
$erl -noinput -sname qcnode1@localhost \
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
