-module(test_erl_node_pingpong).
-compile(export_all).


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
    
		    

    {p, OtherNode} ! Term,
    receive
	Term ->
	    io:format("Ok, got term ~p~n",[Term]),
	    {p, OtherNode} ! LargeTerm,
	    receive
		LargeTerm -> io:format("Ok, got term ~p~n",[LargeTerm]);
		Y    -> io:format("Oops, got ~p~n",[Y])
	    after 10000 ->
		    io:format("Timeout2~n")
	    end;
	X ->
	    io:format("Oops, got ~p~n"++
		      "fun_info:~p~n"++
		      "term: ~p~n",
		      [X, erlang:fun_info(F), term_to_binary(F)])
    after 10000 ->
	    io:format("Timeout~n")
    end.
