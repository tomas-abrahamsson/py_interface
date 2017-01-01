%%% py_interface -- A Python-implementation of an Erlang node
%%%
%%% $Id$
%%%
%%% Copyright (C) 2017  Tomas Abrahamsson
%%%
%%% Author: Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% This file is part of the Py-Interface library
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Library General Public
%%% License as published by the Free Software Foundation; either
%%% version 2 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Library General Public License for more details.
%%%
%%% You should have received a copy of the GNU Library General Public
%%% License along with this library; if not, write to the Free
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

%%% py_node_mgr.py -- manage a python nodes
-module(py_node_mgr).

-export([pingpong_terms/1, pingpong_terms/2]).

-export([ensure_started/0, ensure_started/1]).
-export([ensure_stopped/0, ensure_stopped/1]).
-export([get_output/0, get_output/1]).
-export([set_master/1, set_master/2]).
-export([pingpong_term_with_started/2]).
-export([get_state/0, get_state/1]). % for debugging

-export([start_py_node/1]). % for debugging
-export([stop_py_node/1]). % for debugging

-include_lib("eunit/include/eunit.hrl").

%% one-shot-api
pingpong_terms(Terms) -> pingpong_terms(Terms, []).
pingpong_terms(Terms, Opts) ->
    {ok, NodeName} = ensure_started(),
    set_master(infinity_sleeper()),
    try
        [pingpong_term_with_started(NodeName, Term) || Term <- Terms],
        ok
    catch Class:Reason ->
            St = erlang:get_stacktrace(),
            ensure_stopped(),
            case Reason of
                {_, Infos} when is_list(Infos) ->
                    Output = proplists:get_value(output, Infos),
                    case proplists:get_bool(quiet, Opts) of
                        true ->
                            ok;
                        false ->
                            io:format(
                              user,
                              "~nPyOut = vvvv------------~n"
                              "~s~n"
                              "^^^^--------------------~n",
                              [Output])
                    end;
                _ ->
                    ok
            end,
            erlang:raise(Class,Reason,St)
    end.

infinity_sleeper() ->
    case whereis(infinity_sleeper) of
        undefined ->
            P = spawn(timer,sleep,[infinity]),
            register(infinity_sleeper, P),
            P;
        P when is_pid(P) ->
            P
    end.

%% ------------------
%% Server-API

%% Monitor first starter, terminate if it dies.
-spec ensure_started() -> {ok, NodeName::atom()}.
ensure_started() -> ensure_started([]).
ensure_started(Opts) ->
    MgrName = mgr_name(Opts),
    ensure_mgr_process_running(MgrName, Opts),
    call(MgrName, ensure_started). % to get node name

-spec ensure_stopped() -> ok.
ensure_stopped() -> ensure_stopped([]).
ensure_stopped(Opts) ->
    try
        call(mgr_name(Opts), ensure_stopped)
    catch error:badarg ->
            %% message sending to ?SERVER presumably failed, hence stopped
            ok
    end.

-spec get_output() -> binary().
get_output() -> get_output([]).
get_output(Opts) ->
    call(mgr_name(Opts), get_output).

%% Monitor NewMaster, terminate if it dies.
-spec set_master(pid()) -> ok.
set_master(NewMaster) -> set_master(NewMaster, []).
set_master(NewMaster, Opts) ->
    call(mgr_name(Opts), {set_master, NewMaster}).

-spec pingpong_term_with_started(atom(), term()) -> ok.
pingpong_term_with_started(NodeName, What) ->
    pingpong_term_with_started(NodeName, What, []).
pingpong_term_with_started(NodeName, Term, Opts) ->
    Dest = proplists:get_value(py_pid, Opts, {p, NodeName}),
    Dest ! {self(), Term},
    receive
        {OtherP, Term} when is_pid(OtherP), node(OtherP) /= node() ->
            _ = get_output(), % drop it so we don't acc to much, if later fail
            ok;
        {OtherP, OtherTerm} when is_pid(OtherP), node(OtherP) /= node() ->
            Output = get_output(),
            error({received_unexpected, [{got, OtherTerm}, {sent, Term},
                                         {output, Output}]});
        Other ->
            Output = get_output(),
            error({received_unexpected, [{got, Other}, {sent, Term},
                                         {output, Output}]})
    after 10000 ->
            error({timeout, [{sent, Term},
                             {output, get_output()}]})
    end.

get_state() -> get_state([]).
get_state(Opts) ->
    call(mgr_name(Opts), get_state).

%% -------------------
ensure_mgr_process_running(Name, Opts) ->
    case whereis(Name) of
        undefined ->
            Master = self(),
            ok = proc_lib:start(
                   erlang, apply,
                   [fun() ->
                            try register(Name, self()) of
                                _ ->
                                    init(Master, Opts)
                            catch error:badarg ->
                                    ok %% startup-race
                            end
                    end, []]);
        P when is_pid(P) ->
            ok
    end.

-record(pynode, {port      :: port(),
                 node_name :: atom(),
                 os_pid    :: non_neg_integer()}).
-record(state, {py_node :: #pynode{},
                mref :: reference(),
                opts :: [term()]}).

init(Master, Opts) ->
    MRef = monitor(process, Master),
    {ok, PyNode} = start_py_node(Opts),
    proc_lib:init_ack(ok),
    loop(#state{py_node = PyNode, mref = MRef, opts = Opts}, []).

%% State: py_node is running normally
loop(#state{py_node=#pynode{port=Port, node_name=NodeName}=PyNode,
            mref=MRef}=State,
     AccOutput) ->
    receive
        {Port, {data, Txt}} ->
            loop(State, [Txt | AccOutput]);
        {Port, {exit_status, _ExitStatus}} ->
            loop_terminated(State, io_finalize(AccOutput));
        {'DOWN', MRef, _, _, _} ->
            stop_py_node(PyNode),
            exit(normal);
        {call, Req, From} ->
            case Req of
                {set_master, NewMaster} ->
                    demonitor(MRef, [flush]),
                    MRef1 = monitor(process, NewMaster),
                    reply(From, ok),
                    loop(State#state{mref = MRef1}, AccOutput);
                get_output ->
                    case sync_py_node(PyNode) of
                        {ok, Resp} ->
                            reply(From, io_finalize([Resp | AccOutput])),
                            loop(State, []);
                        {error, Output} ->
                            reply(From, io_finalize([Output | AccOutput])),
                            loop_terminated(State, [])
                    end;
                ensure_started ->
                    reply(From, {ok, NodeName}),
                    loop(State, AccOutput);
                ensure_stopped ->
                    stop_py_node(PyNode),
                    loop_stopping(State, From, AccOutput);
                get_state ->
                    reply(From, {loop, State, AccOutput}),
                    loop(State, AccOutput)
            end;
        X ->
            io:format("Unexpected msg (ignored):~n  ~p~n", [X]),
            loop(State, AccOutput)
    end.

%% State: py_node has terminated
loop_terminated(#state{mref=MRef, opts=Opts}=State, Output) ->
    receive
        {'DOWN', MRef, _, _, _} ->
            exit(normal);
        {call, Req, From} ->
            case Req of
                {set_master, NewMaster} ->
                    demonitor(MRef, [flush]),
                    MRef1 = monitor(process, NewMaster),
                    reply(From, ok),
                    loop_terminated(State#state{mref = MRef1}, Output);
                get_output ->
                    reply(From, Output),
                    loop_terminated(State, <<>>);
                ensure_started ->
                    {ok, #pynode{node_name=NodeName}=PyNode} =
                        start_py_node(Opts),
                    reply(From, {ok, NodeName}),
                    loop(State#state{py_node = PyNode}, []);
                ensure_stopped ->
                    reply(From, ok),
                    loop_terminated(State, Output);
                get_state ->
                    reply(From, {terminated, State, Output}),
                    loop_terminated(State, Output)
            end;
        X ->
            io:format("Unexpected msg (ignored):~n  ~p~n", [X]),
            loop_terminated(State, Output)
    end.

%% Collect pending output, then go to terminated
loop_stopping(#state{py_node=#pynode{port=Port},
                     mref=MRef}=State,
              From,
              AccOutput) ->
    receive
        {Port, {data, Txt}} ->
            loop_stopping(State, From, [Txt | AccOutput]);
        {Port, {exit_status, _ExitStatus}} ->
            reply(From, ok),
            loop_terminated(State, io_finalize(AccOutput));
        {'DOWN', MRef, _, _, _} ->
            exit(normal)
    end.

sync_py_node(#pynode{port=Port}=PyNode) ->
    port_command(Port, "\n"),
    read_till_flush(PyNode, []).

read_till_flush(#pynode{port=Port}=PyNode, AccOutput) ->
    receive
        {Port, {data, Txt}} ->
            AllOut = io_finalize([Txt | AccOutput]),
            case binary:match(AllOut, <<"-FLUSH-">>) of
                nomatch ->
                    read_till_flush(PyNode, [AllOut]);
                {_Start, _Len} ->
                    {ok, AllOut}
            end;
        {Port, {exit_status, _ExitStatus}} ->
            {error, io_finalize(AccOutput)}
    end.

io_finalize(Acc) -> iolist_to_binary(lists:reverse(Acc)). 

call(Name, Req) ->
    MRef = monitor(process, Name),
    Name ! {call, Req, {MRef, self()}},
    receive
        {reply, MRef, Res} ->
            demonitor(MRef, [flush]),
            Res;
        {'DOWN', MRef, _, _, Reason} ->
            exit({terminated,Reason})
    end.

reply({Ref, Client}, Res) ->
    Client ! {reply, Ref, Res}.

mgr_name(Opts) ->
    list_to_atom(lists:concat(["mgr_for_", get_py_name(Opts)])).

get_py_name(Opts) ->
    filename:basename(get_py_prog(Opts), ".py").

get_py_prog(Opts) ->
    proplists:get_value(py_prog, Opts, "./pingpong_slave.py").

start_py_node(Opts) ->
    PyName = get_py_name(Opts),
    PyProg = get_py_prog(Opts),
    PyArgs = proplists:get_value(py_args, Opts, []),
    SelfOsPid = os:getpid(),
    NodeName = mk_unused_node_name(lists:concat(
                                     [PyName, "-for-", SelfOsPid, "-"])),
    P = open_port({spawn_executable, PyProg},
                  [use_stdio, stderr_to_stdout, binary,
                   exit_status,
                   {env, [{"PYTHONDONTWRITEBYTECODE", "x"},
                          {"PYTHONUNBUFFERED", "x"}]},
                   {args,["-n", atom_to_list(NodeName),
                          "-c", erlang:get_cookie(),
                          "-d"
                          | PyArgs]}]),
    {os_pid,OsPid} = erlang:port_info(P, os_pid),
    PyNode = #pynode{port      = P,
                     node_name = NodeName,
                     os_pid    = OsPid},
    await_py_node_up(PyNode),
    {ok, PyNode}.

mk_unused_node_name(Prefix) ->
    U = get_unique_integer(),
    Base = lists:concat([Prefix, U]),
    case erl_epmd:names() of
        {ok, LocalNames} ->
            case lists:keymember(Base, 1, LocalNames) of
                true ->
                    mk_unused_node_name(Prefix);
                false ->
                    list_to_atom(lists:concat([Base, "@127.0.0.1"]))
            end;
        _ ->
            list_to_atom(lists:concat([Base, "@127.0.0.1"]))
    end.

await_py_node_up(PyNode) ->
    MaxT = calc_max_t(timer:seconds(5)),
    await_py_node_going_looping(PyNode, MaxT, []),
    await_py_node_in_epmd(PyNode, MaxT).

await_py_node_going_looping(#pynode{port=Port}=PyNode, MaxT, Acc) ->
    case collect_output_aux(Port, Acc) of
        {undefined, Txt} -> % still running
            case binary:match(Txt, <<"-GOING LOOPING-">>) of
                nomatch ->
                    case has_timed_out(MaxT) of
                        true -> error({timeout_awaiting_going_looping,
                                       [{state, still_running},
                                        {output, Txt}]});
                        false ->
                            timer:sleep(25),
                            await_py_node_going_looping(PyNode, MaxT, [Txt])
                    end;
                {_Start, _Len} ->
                    ok
            end;
        {ExitStatus, Txt} ->
            error({py_node_terminated_prematurely,
                   [{exit_status, ExitStatus},
                    {output, Txt}]})
    end.

await_py_node_in_epmd(#pynode{node_name=NodeName}=PyNode, MaxT) ->
    UpToAt = lists:takewhile(fun(C) -> C /= $@ end, atom_to_list(NodeName)),
    {ok, Names} = erl_epmd:names(), % expecting epmd to be started
    case lists:keymember(UpToAt, 1, Names) of
        true ->
            ok;
        false ->
            case has_timed_out(MaxT) of
                true -> error({timeout_awaiting_node_in_epmd,
                               [{names_in_epmd, Names}]});
                false ->
                    timer:sleep(25),
                    await_py_node_in_epmd(PyNode, MaxT)
            end
    end.

stop_py_node(#pynode{os_pid=OsPid}) ->
    os:cmd("kill -9 "++integer_to_list(OsPid)).

collect_output_aux(Port, Acc) ->
    receive
        {Port, {data, Txt}} ->
            collect_output_aux(Port, [Txt | Acc]);
        {Port, {exit_status,ExitStatus}} ->
            {ExitStatus, iolist_to_binary(lists:reverse(Acc))}
    after 50 ->
            {undefined, iolist_to_binary(lists:reverse(Acc))}
    end.

-ifndef(NO_HAVE_ERTS7_TIME_API).
get_unique_integer() ->
    erlang:unique_integer([positive]).

calc_max_t(MillisFromNow) ->
    erlang:monotonic_time(millisecond) + MillisFromNow.

has_timed_out(MaxT) ->
    erlang:monotonic_time(millisecond) > MaxT.

-else. % NO_HAVE_ERTS7_TIME_API
get_unique_integer() ->
    {N1,N2,N3} = now(),
    (N1 * 1000000 + N2) * 1000000 + N3.

calc_max_t(MaxT) ->
    {now(),MaxT}.

has_timed_out({T0,MaxT}) ->
    (timer:now_diff(now(),T0) / 1000) > MaxT.

-endif. % NO_HAVE_ERTS7_TIME_API
