-module(board).
-export([start/2, init/2]).

-export([disconnect/1, exec/2, exec/3]).

-record(state, {ssh_handle :: ct_ssh:handle(),
                board :: atom(),
                config :: any()}).

-type board() :: pid().

-spec start(atom(), proplists:proplists()) -> board().
start(KeyOrName,Config) ->
    spawn(?MODULE, init, [KeyOrName, Config]).


-spec disconnect(board()) -> ok.
disconnect(Board) ->
    call(Board, disconnect).


-spec exec(board(), string()) -> {ok, list()}.
exec(Board, Command) ->
    call(Board, {exec, Command}).


-spec exec(board(), string(), string()) -> {ok, list()}.
exec(Board, Command, Arguments) ->
    call(Board, {exec, {Command, Arguments}}).


init(KeyOrName, Config) ->

    {ok, SshHandle} = ct_ssh:connect(KeyOrName),

    loop(#state{ ssh_handle = SshHandle, board = KeyOrName, config = Config}).


loop(#state{ssh_handle = SshHandle, config = Config, board = Board} = State) ->
    receive
        {request, {_Ref, _From} = From, disconnect} ->
            ct:pal(default, 50, "~p: disconnect", [Board]),

            ok = ct_ssh:disconnect(SshHandle),
            reply(From, {board, ok}),
            loop(State#state{ssh_handle = undefined});

        {request, {_Ref, _From} = From, {exec, Command}} ->
            ExecCommand = get_command(Config, Command),
            Result = ct_ssh:exec(SshHandle, ExecCommand, 5000),

            ct:pal(default, 50, "~p: exec command ~p result '~p'",
             [Board, Command, Result]),
            
            reply(From, {board, Result}),
            loop(State)
    end.


get_command(Config, {Command, Arguments}) when is_atom(Command) ->
    C = proplists:get_value(Command, Config),
    R = lists:join(" ", [C, Arguments]),
    R;

get_command(Config, Command) when is_atom(Command) ->
    C = proplists:get_value(Command, Config),
    C;

get_command(_Config, Command) when is_list(Command) ->
    Command.


call(Board, Msg) ->
    Ref = erlang:monitor(process, Board),
    Board ! {request, {Ref, self()}, Msg},
    receive
        {reply, Ref, {board, Result}} ->
            Result;
        {'DOWN', Ref, process, _Pid, _Reason} ->
            throw({board, down})
    after 15000 ->
        throw({board, timeout})
    end.


reply({Ref, To}, Reply) ->
    To ! {reply, Ref, Reply}.