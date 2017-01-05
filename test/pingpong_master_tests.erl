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

%%% pingpong_master_tests.py -- unit tests for ping-pong-ing msgs, master side
-module(pingpong_master_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 30).

-define(pp(Expr), {timeout, ?TIMEOUT, {spawn, fun() -> Expr end}}).

-define(f(Fmt,Args), lists:flatten(io_lib:format(Fmt, Args))).

-import(py_node_mgr, [pingpong_terms/1, pingpong_terms/2]).
-import(py_node_mgr, [pingpong_terms_2/2]).

check_can_fail_test_() ->
    ?pp(?assertError({received_unexpected,
                      [{got,  ok_deliberately_failed_as_you_wished},
                       {sent, do_deliberately_fail} | _]},
                     pingpong_terms([do_deliberately_fail], [quiet]))).

small_integer_test_() ->
    ?pp(pingpong_terms([1, 255])).

integer_test_() ->
    ?pp(pingpong_terms([256, 16#7fffffff])).

negative_integer_test_() ->
    ?pp(pingpong_terms([-1, -256, -16#7fffffff, -16#80000000])).

float_test_() ->
    ?pp(pingpong_terms([1.125])).

atom_test_() ->
    ?pp(pingpong_terms([atom, ''])).

reference_test_() ->
    ?pp(pingpong_terms([make_ref()])).

port_test_() ->
    case erlang:ports() of
        [Port | _] -> ?pp(pingpong_terms([Port]));
        _ -> {timeout, ?TIMEOUT, fun() -> ok end}
    end.

pid_test_() ->
    ?pp(pingpong_terms([self()])).

small_tuple_test_() -> %% up to 255 elements
    ?pp(pingpong_terms([{}, {1}, {1,2,3,4}])).

nil_test_() ->
    ?pp(pingpong_terms([[]])).

string_test_() ->
    ?pp(pingpong_terms(["abc", lists:duplicate(3000, $a)])).

list_test_() ->
    ?pp(pingpong_terms([[1,2,3]])).

improper_list_test_() ->
    ?pp(pingpong_terms([[a|b]])).

binary_test_() ->
    ?pp(pingpong_terms([<<>>, <<"abc">>])).

bit_binary_test_() ->
    ?pp(pingpong_terms([<<1:2>>])).

small_big_test_() ->
    ?pp(pingpong_terms([2 bsl 33])).

negative_small_big_test_() ->
    ?pp(pingpong_terms([-16#80000001])).

new_fun_test_() ->
    ?pp(pingpong_terms([fun() -> ok end,
                        fun dummy/1])).

dummy(X) -> X.

export_test_() ->
    ?pp(pingpong_terms([fun lists:usort/1])).

-ifndef(NO_HAVE_MAPS).
map_test_() ->
    ?pp(pingpong_terms([#{}, #{a=>1}])).

map_with_pymutable_obj_as_key_test_() ->
    ?pp(pingpong_terms([%% A list is mutable in Python, hence cannot
                        %% normally occur as a dict key.
                        #{[1,2] => a}, #{"abc" => [1,2]},
                        %% an ErlMapKey() object:
                        #{make_ref() => 0}])).
-endif. % NO_HAVE_MAPS

large_tuple_test_() -> % >255 elements
    ?pp(pingpong_terms([list_to_tuple(lists:seq(0,257))])).

large_big_ext_test_() -> % needs > 255 bytes, positive and negative
    ?pp(pingpong_terms([2 bsl (256*8), -(2 bsl (256*8))])).

py_node_connects_test_() ->
    ?pp(py_node_connects_aux()).

py_node_connects_aux() ->
    ProcName = mk_dummy_proc_name(),
    register(ProcName, self()),
    U = get_unique_integer(),
    A = list_to_atom(lists:concat(["t_msg_",U])),
    Opts = [{py_prog, "./out_connecting.py"},
            {py_args, [atom_to_list(ProcName),
                       atom_to_list(node()),
                       atom_to_list(A)]}],
    py_node_mgr:ensure_started(Opts),
    receive
        {PyPid, A} ->
            pingpong_terms([abc123], [{py_pid, PyPid} | Opts])
    after 10000 ->
            error({timeout_awaiting_msg_from_out_connecting_py_mode,
                   [{got, flush()},
                    {opts, Opts},
                    {output, py_node_mgr:get_output(Opts)}]})
    end.

py_node_runs_rpc_test_() ->
    ?pp(py_node_runs_rpc_aux()).

py_node_runs_rpc_aux() ->
    ProcName = mk_dummy_proc_name(),
    register(ProcName, self()),
    ToSum = [1,2,3,4],
    Sum = lists:sum(ToSum),
    Opts = [{py_prog, "./rpc_caller.py"},
            {py_args, ["-r", atom_to_list(ProcName),
                       atom_to_list(node()),
                       "lists",
                       "sum",
                       ?f("[~p]", [ToSum])]}],
    py_node_mgr:ensure_started(Opts),
    receive
        {rpc_result, Sum} ->
            ok;
        {rpc_result, Other} ->
            error({got_unexpected_from_rpc_caller_py_mode,
                   [{got, Other},
                    {opts, Opts},
                    {output, py_node_mgr:get_output(Opts)},
                    {flush, flush()}]})
    after 10000 ->
            error({timeout_awaiting_rpc_result_from_rpc_caller_py_mode,
                   [{got, flush()},
                    {opts, Opts},
                    {output, py_node_mgr:get_output(Opts)}]})
    end.

%% ------------

mk_dummy_proc_name() ->
    U = get_unique_integer(),
    Cand = list_to_atom(lists:concat([?MODULE,"_",U])),
    case whereis(Cand) of
        P when is_pid(P) -> mk_dummy_proc_name();
        undefined -> Cand
    end.

flush() ->
    receive
        Msg ->
            [Msg | flush()]
    after 0 ->
            []
    end.

-ifndef(NO_HAVE_ERTS7_TIME_API).
get_unique_integer() ->
    erlang:unique_integer([positive]).
-else. % NO_HAVE_ERTS7_TIME_API
get_unique_integer() ->
    {N1,N2,N3} = now(),
    (N1 * 1000000 + N2) * 1000000 + N3.
-endif. % NO_HAVE_ERTS7_TIME_API
