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

%%% pingpong_master_tests.py -- quickcheck/proper tests
%%% for ping-pong-ing msgs, master side
-module(qc_pingpong_master_tests).

-compile(export_all).

-define(TIMEOUT, 180). % extra time needed in case of shrinking, on slow host

-export([prop_same_term_returns_after_roundtrip/1]).

-ifdef(HAVE_EQC).
-include_lib("eqc/include/eqc.hrl").
-define(qc,eqc).
-else.
-ifdef(HAVE_PROPER).
-include_lib("proper/include/proper.hrl").
-define(qc,proper).
-endif.
-endif.

-include_lib("eunit/include/eunit.hrl").

prop_same_term_returns_after_roundtrip_test_() ->
    {timeout,?TIMEOUT,
     fun() ->
             py_node_mgr:ensure_started(),
             ?assert(
                ?qc:quickcheck(
                   ?qc:numtests(
                      50,
                      prop_same_term_returns_after_roundtrip(self())))),
             py_node_mgr:ensure_stopped(),
             ok
     end}.

prop_same_term_returns_after_roundtrip(Master) ->
    ?FORALL(Term, t_term(),
            begin
                {_P,MRef} = do_spawn_pingponger(Master, Term),
                do_collect_pingponger_reply(MRef)
            end).

do_spawn_pingponger(Master, Term) ->
    spawn_monitor(
      fun() ->
              try
                  {ok,NodeName} = py_node_mgr:ensure_started(),
                  py_node_mgr:set_master(Master),
                  py_node_mgr:pingpong_term_with_started(NodeName, Term)
              catch Class:Reason ->
                      St = erlang:get_stacktrace(),
                      py_node_mgr:ensure_stopped(),
                      exit({fail,Term,Class,Reason,St})
              end
      end).

do_collect_pingponger_reply(MRef) ->
    receive
        {'DOWN', MRef, _, _, Reason} ->
            case Reason of
                normal ->
                    true;
                {fail,_Term,Class,Reason2,St} ->
                    erlang:raise(Class,Reason2,St)
            end
    end.

t_term() ->
    oneof([t_term2(),
           list(t_term2()),
           t_tuple(n_uint(14), t_term2())]).

t_term2() ->
    oneof([int(), n_uint(32), n_sint(32), largeint(),
           real(),
           t_atom(),
           t_ref(),
           t_pid(),
           t_string(),
           binary(),
           bitstring(),
           t_fun(),
           t_improper_list(),
           t_port(),
           t_map()]).

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

t_fun() ->
    oneof([fun dummy/0,
           fun() -> ok end,
           fun lists:reverse/1]).

dummy() ->
    ok.

t_improper_list() ->
    ?LET({E1,E2}, {t_atom(), t_atom()}, [E1|E2]).

-ifndef(NO_HAVE_MAPS).
t_map() ->
    ?LET(NumElems, n_uint(2),
         ?LET({Keys, Values}, {vector(NumElems, t_term2()),
                               vector(NumElems, t_term2())},
              maps:from_list(lists:zip(Keys, Values)))).
-else. % NO_HAVE_MAPS
t_map() ->
    t_atom(). % whatever
-endif. % NO_HAVE_MAPS
