%% A test case for testing packing/unpacking of erlang-terms:
%%
%% See run_test_erl_node_pingpong_qc.sh for how to run it.
%%
%% Use quickcheck (eqcmini) to generate a lot of various values
%% to send to the python node and then receive back again.
%%
%% The eqcmini is a free (as in free of charge) version of quickcheck:
%% http://www.quviq.com/downloads/eqcmini.zip
%% More info here: http://www.quviq.com/news100621.html
%%
%% This might also work with PropEr, https://github.com/manopapad/proper
%%
-module(test_erl_node_pingpong_qc).

-export([start_halt/0, start_halt/1]).
-export([start/0, start/1]).

-export([prop_same_term_returns_after_roundtrip/0]).
-export([prop_same_term_returns_after_roundtrip/1]).

-define(PYNODENAME_HOLDER,   list_to_atom(atom_to_list(?MODULE)++"_holder")).
-define(PYNODENAME_HOLDER_T, list_to_atom(atom_to_list(?MODULE)++"_holder_t")).
-define(MAX_WAIT_PYNODE_UP, 20). % seconds

-ifdef(HAVE_EQC).
-include_lib("eqc/include/eqc.hrl").
-define(qc,eqc).
-else.
-ifdef(HAVE_PROPER).
-include_lib("proper/include/proper.hrl").
-define(qc,proper).
-endif.
-endif.

start_halt() -> halt_with_status(start()).

start_halt(Args) -> halt_with_status(start(Args)).

halt_with_status(ok)  -> halt(0);
halt_with_status(Err) -> io:format("ERR=~p~n", [Err]), halt(1).

start() ->
    start(['py_interface_test_qc@localhost']).

start([PyNodeName, PyCmdA]) ->
    ensure_pynodename_holder_started(PyNodeName, atom_to_list(PyCmdA)),
    io:format("Checking for Python node (~p) liveness...~n", [PyNodeName]),
    case wait_until_node_available(PyNodeName, ?MAX_WAIT_PYNODE_UP) of
        ok ->
            io:format("Running tests...~n"),
            try ?qc:module(?MODULE) of
                []  -> ok;
                Err -> {error, Err}
            catch Class:Reason ->
                    {error, {Class,Reason,erlang:get_stacktrace()}}
            end;
        {error, Reason} ->
            io:format("Failed to find the python node ~p~n  ~p~n",
                      [PyNodeName, Reason]),
            {error,{connection_failure,Reason}}
    end.

prop_same_term_returns_after_roundtrip() ->
    ?qc:numtests(500,
                 prop_same_term_returns_after_roundtrip(get_pynodename())).

prop_same_term_returns_after_roundtrip(PyNodeName) ->
    ?FORALL(Term, term(),
            begin
                ensure_py_node_running(),
                ensure_node_monitored(PyNodeName),
                Me = self(),
                {p, PyNodeName} ! {Me, Term},
                Received =
                    receive
                        {Me, Term2} ->
                            {got,Term2};
                        {nodedown, PyNodeName, _} ->
                            case flush_mbox() of
                                [] -> nodedown;
                                Ms -> {nodedown,{but_got_msgs,Ms}}
                            end
                    end,
                ?WHENFAIL(if is_tuple(Term) ->
                                  io:format("TupleTerm (~p elems)=~p~nGot=~p~n", [tuple_size(Term), Term, Received]);
                             true ->
                                  io:format("Term=~p~nGot=~p~n", [Term, Received])
                          end,
                          case Received of
                              nodedown     -> false;
                              {nodedown,_} -> false;
                              {got,X}      -> X =:= Term
                          end)
            end).

term() ->
    oneof([term2(),
           list(term2()),
           t_tuple(n_uint(14), term2())]).

term2() ->
    oneof([int(), n_uint(32), n_sint(32), largeint(),
           real(),
           t_atom(),
           t_ref(),
           t_pid(),
           t_string(),
           binary(),
           %% bitstring(), not supported yet
           %% some-function...
           %% improper list
           %% m:f/a export
           t_port()]).

n_sint(Base) ->
    oneof([n_uint(Base), ?LET(X, n_uint(Base), -X)]).

n_uint(Base) ->
    oneof([choose(0, pow2(B)-1) || B <- lists:seq(1, Base)]).

pow2(0)            -> 1;
pow2(N) when N > 0 -> 2*pow2(N-1);
pow2(N) when N < 0 -> 1/pow2(-N).

t_tuple(SizeG, G) ->
    ?LET(NumElems, SizeG,
         ?LET(L, vector(NumElems, G), list_to_tuple(L))).

t_atom() ->
    oneof([a0,  a1,  a2,  a3,  a4,  a5,  a6,  a7,  a8,  a9,
           a10, a11, a12, a13, a14, a15, a16, a17, a18, a19,
           a20, a21, a22, a23, a24, a25, a26, a27, a28, a29,
           x]).

t_ref() ->
    ?LET(_, int(), make_ref()).

t_pid() ->
    ?LET(_, int(), self()).

t_port() ->
    elements(erlang:ports()).


t_string() ->
    list(n_uint(8)).


%% auxiliaries --------------------------------------------------------

ensure_py_node_running() ->
    PyNodeName = get_pynodename(),
    case wait_until_node_available(PyNodeName, 0.1) of
        ok ->
            ok;
        {error,_} ->
            Cmd = get_pynodecmd() ++ " &",
            spawn(fun() -> Cmd2 = Cmd++ "\necho $! > pynode_pid",
                           case os:cmd(Cmd2) of
                               "" -> ok;
                               X  -> io:format("Cmd ~p~n==> ~p~n", [Cmd2, X])
                           end
                  end),
            case wait_until_node_available(PyNodeName, ?MAX_WAIT_PYNODE_UP) of
                ok ->
                    flush_any_nodeup(),
                    timer:sleep(300), %% Give it some time to start properly
                    ok;
                {error, Reason} ->
                    exit({e2,Reason})
            end
    end.

ensure_node_monitored(_PyNodeName) ->
    case get(has_monitored) of
        undefined ->
            net_kernel:monitor_nodes(true, [{node_type, hidden}]),
            put(has_monitored, true);
        true ->
            ok
    end.

ensure_pynodename_holder_started(PyNodeName, PyNodeCmd) ->
    case whereis(?PYNODENAME_HOLDER) of
        undefined ->
            Master = self(),
            {P, MRef} = erlang:spawn_monitor(
                          fun() ->
                                  T = ets:new(?PYNODENAME_HOLDER_T,
                                              [set,named_table]),
                                  ets:insert(T, {pynodename,PyNodeName}),
                                  ets:insert(T, {pynodecmd,PyNodeCmd}),
                                  Master ! {self(), started},
                                  timer:sleep(infinity)
                          end),
            receive
                {'DOWN',MRef,_,_,Reason} ->
                    exit(Reason);
                {P, started} ->
                    erlang:demonitor(MRef,[flush]),
                    ok
            end;
        P when is_pid(P) ->
            ok
    end.

get_pynodename() ->
    [{pynodename,PyNodeName}] = ets:lookup(?PYNODENAME_HOLDER_T, pynodename),
    PyNodeName.

get_pynodecmd() ->
    [{pynodecmd,PyNodeCmd}] = ets:lookup(?PYNODENAME_HOLDER_T, pynodecmd),
    PyNodeCmd.

wait_until_node_available(NodeName, MaxT) ->
    wait_until_node_available(NodeName, MaxT, _T0=now()).

wait_until_node_available(NodeName, MaxT, T0) ->
    NStr = atom_to_list(NodeName),
    {ok,NamePorts} = erl_epmd:names(),
    case [Name || {Name,_P} <- NamePorts, lists:prefix(Name, NStr)] of
        [] ->
            SecondsSinceT0 = timer:now_diff(now(), T0) / 1000000,
            if SecondsSinceT0 > MaxT ->
                    {error, {timeout_waiting_for_presence_in_epmd,
                             {NodeName, NamePorts}}};
               true ->
                    timer:sleep(100),
                    wait_until_node_available(NodeName, MaxT, T0)
            end;
        [_NodeNameInEpmd] ->
            ok
    end.

flush_mbox() ->
    receive
        X -> [X | flush_mbox()]
    after 0 ->
            []
    end.

flush_any_nodeup() ->
    N = get_pynodename(),
    receive
        {nodeup,N,_} -> flush_any_nodeup()
    after 0 ->
            ok
    end.
