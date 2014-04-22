%% A test case for testing packing/unpacking of erlang-terms:
%%
%% See run_test_erl_node_pingpong.sh for how to run it.
%%
%% A message is sent from an erlang node to a python node.
%% That message is echoed back to the erlang node, which checks
%% if the received message matches the original message.
%%
-module(test_erl_node_pingpong).

-export([start_halt/0, start_halt/1]).
-export([start/0, start/1]).


start_halt() -> halt_with_status(start()).

start_halt(Args) -> halt_with_status(start(Args)).

halt_with_status(ok) -> halt(0);
halt_with_status(_)  -> halt(1).

start() ->
    start(['py_interface_test@localhost']).

start([OtherNode]) ->
    Term = {self(), [1,         % small_integer_ext
                     255,       % small_integer_ext
                     256,       % integer_ext
                     16#7fffffff, % integer_ext
                     -1,        % negative integer_ext
                     -256,      % negative integer_ext
                     -16#7fffffff,% negative integer_ext
                     -16#80000000,% negative integer_ext
                     1.125,     % float_ext
                     atom_ext,  % atom_ext
                     '',        % atom_ext
                     make_ref(),% new_reference_ext (reference_ext)
                     fixme,     % port_ext
                     self(),    % pid_ext
                     {},        % small_tuple_ext (up to 255 elements)
                     {1},       % small_tuple_ext (up to 255 elements)
                     {1,2,3,4}, % small_tuple_ext (up to 255 elements)
                     [],        % nil_ext
                     "abc",     % string_ext (up to 64k chars)
                     [1,2,3],   % list_ext
                                % fixme: make test for improper lists
                     <<>>,      % binary_ext
                     <<"abc">>, % binary_ext
                     <<1:2>>,   % bit_binary_ext
                     2 bsl 33,  % small_big_ext
                     -16#80000001,% negative small_big_ext
                     fun() -> ok end, % new_fun_ext
                     fun dummy/1, % new_fun_ext
                     fun lists:usort/1, % export_ext
                     some_map(),
                     ' $end$ '
                    ]
           },
    LargeTerm = {self(),
                 [%% large_tuple_ext (>255 elements)
                  list_to_tuple(lists:seq(0,257)),
                  %% large_big_ext (needs > 255 bytes), positive and negative
                  2 bsl (256*8),
                  -(2 bsl (256*8)),
                  %% string_ext
                  lists:duplicate(3000, $a),
                  ' $endlarge$ '
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
                             io:format("Test succeeded!~n"),
                             ok;
                Y    -> io:format("Oops, got ~p~n",[Y]),
                        io:format("Test failed.~n"),
                        error
            after 10000 ->
                    io:format("Timeout2~n"),
                    io:format("Test failed.~n"),
                    error
            end;
        X ->
            io:format("Oops, expected:~n  ~p~n"++
                      "got:~n  ~p~n", [Term, X]),
            io:format("Test failed.~n"),
            error
    after 10000 ->
            io:format("Timeout~n"),
            io:format("Test failed.~n"),
            error
    end.

dummy(_) ->
    ok.

-ifdef(HAVE_MAPS).
some_map() ->
    #{a=>1}.
-else.
some_map() ->
    no_maps_in_this_release_of_erlang.
-endif.
