%% A test case for testing packing/unpacking of erlang-terms:
%%
%% See run_test_erl_node_pingpong.sh for how to run it.
%%
%% A message is sent from an erlang node to a python node.
%% That message is echoed back to the erlang node, which checks
%% if the received message matches the original message.
%%
-module(test_erl_node_pingpong).

-export([start/0, start/1]).


start() ->
    start(['py_interface_test@localhost']).
    
start([OtherNode]) ->
    Term = {self(), [1,		% small_integer_ext
		     255,	% small_integer_ext
		     256,	% integer_ext
		     1.1,	% float_ext
		     atom_ext,	% atom_ext
		     '',	% atom_ext
		     make_ref(),% new_reference_ext (reference_ext)
		     fixme,	% port_ext
		     self(),	% pid_ext
		     {},	% small_tuple_ext (up to 255 elements)
		     {1},	% small_tuple_ext (up to 255 elements)
		     {1,2,3,4},	% small_tuple_ext (up to 255 elements)
		     [],	% nil_ext
		     "abc",	% string_ext (up to 65k chars)
		     [1,2,3],	% list_ext
				% fixme: make test for improper lists
		     <<>>,	% binary_ext
		     <<"abc">>,	% binary_ext
		     2 bsl 33,	% small_big_ext
		     fixme,	% fixme: flag stuff needed to get funs to work
		     '$end$'
		    ]
	   },
    LargeTerm = {self(),
		 [%% large_tuple_ext (>255 elements)
		  list_to_tuple(lists:seq(0,257)),
		  %% large_big_ext (needs > 255 bytes),
		  2 bsl (256*8),
		  %% string_ext
		  lists:duplicate(3000, $a),
		  '$endlarge$'
		 ]},
    
		    
    io:format("Sending message...~n"),
    {p, OtherNode} ! Term,
    io:format("Sending message...done~n"),
    io:format("Waiting for answer...~n"),
    receive
	Term ->
	    io:format("Ok, got term ~p~n",[Term]),
	    io:format("Sending larger message...~n"),
	    {p, OtherNode} ! LargeTerm,
	    io:format("Sending larger message...done~n"),
	    io:format("Waiting for answer again...~n"),
	    receive
		LargeTerm -> io:format("Ok, got term ~p~n",[LargeTerm]),
			     io:format("Test succeeded!~n");
		Y    -> io:format("Oops, got ~p~n",[Y]),
			io:format("Test failed.~n")
	    after 10000 ->
		    io:format("Timeout2~n"),
		    io:format("Test failed.~n")
	    end;
	X ->
	    io:format("Oops, expected:~n  ~p~n"++
		      "got:~n  ~p~n", [X, X]),
	    io:format("Test failed.~n")
    after 10000 ->
	    io:format("Timeout~n"),
	    io:format("Test failed.~n")
    end.
