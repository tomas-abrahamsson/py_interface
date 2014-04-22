#! /bin/sh

# A simple demo to show remote execution
if [ x"$1" = x"-d" ]
then
    debug="-d"
    shift
fi

if [ ! -f ./test_remote_exec.py ]
then
    echo "Run me in the examples directory!"
    echo "Expected test_remote_exec.py to be in the current working directory"
    echo "in order to get python path setup correct. Exiting."
    exit 1
fi


erl=${ERL:-erl}
epmd=${EPMD:-epmd}

echo "Starting a backgrounded erlang node..."
$erl -setcookie more-cookies -sname enode@localhost -noinput \
    -eval 'timer:sleep(15000), erlang:halt().' &
erlnode=$!
echo "Starting a backgrounded erlang node...$erlnode"
printf "Waiting the backgrounded node to register to the epmd..."
registered=false
num_tries=0
while [ $registered = false -a $num_tries -lt 10 ]
do
    if ($epmd -names | egrep '^name enode at port' >/dev/null)
    then
        registered=true
    fi
    sleep 1
    printf "."
    num_tries=`expr $num_tries + 1`
done
echo
echo "Waiting the backgrounded node to register to the epmd...$registered"

trap "/bin/kill $erlnode 2>/dev/null || true; exit" 0

echo
echo "Will now ask the remote erlang-node to print something."
echo "-------------------------------------------------------"
# Now start the pythonnode
PYTHONPATH=..:$PYTHONPATH ./test_remote_exec.py \
    $debug -n pynode1@localhost -c more-cookies -t 5 \
    enode@localhost io format '"Erlang side printing: ~p~n"' "['xyz']"
echo
echo "Will now halt the remote erlang-node."
echo "-------------------------------------"
PYTHONPATH=..:$PYTHONPATH ./test_remote_exec.py \
    $debug -n pynode2@localhost -c more-cookies -t 5 \
    enode@localhost erlang halt
