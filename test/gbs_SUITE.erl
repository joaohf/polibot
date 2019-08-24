-module(gbs_SUITE).
-compile(export_all).
-compile(nowarn_export_all).
-include_lib("common_test/include/ct.hrl").


suite() ->
    % Make SSH settings from config files, sshconfig.cfg, available
    % in the suite
    [{require, circle0},
     {require, triangle0},
     {require, square0},
     {require, square1},
     {require, circle0_config},
     {require, triangle0_config},
     {require, square0_config},
     {require, square1_config}].


init_per_suite(Config) ->
    Config.


end_per_suite(Config) ->
    Config.


init_per_group(init_test, Config) ->
    Boards = boards_start(Config),
    [{boards, Boards} | Config];

init_per_group(leds_test, Config) ->
    Boards = boards_start(Config),
    [{boards, Boards} | Config];

init_per_group(_GroupName, Config) ->
    Config.


end_per_group(init_test, Config) ->
    boards_stop(Config),
    ok;

end_per_group(leds_test, Config) ->
    boards_stop(Config),
    ok;

end_per_group(_GroupName, _Config) ->
    ok.


init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    ok.


groups() ->
    [{bringup_test, [], [bringup]},
     {init_test, [], [board_init]},
     {leds_test, [], [leds_on]}].


all() ->
    [{group, bringup_test},
     {group, init_test},
     {group, leds_test}].


bringup(_Config) ->

    C0Config = ct:get_config(circle0_config),
    T0Config = ct:get_config(triangle0_config),
    S0Config = ct:get_config(square0_config),
    S1Config = ct:get_config(square1_config),

    C0Board = board:start(circle0, C0Config),
    T0Board = board:start(triangle0, T0Config),
    S0Board = board:start(square0, S0Config),
    S1Board = board:start(square1, S1Config),

    Boards = [C0Board, T0Board, S0Board, S1Board],

    [begin
        {ok, Result} = board:exec(Board, "uname -a"),
        ct:pal(default, 50, "~p: exec result ~p", [Board, Result])
     end || Board <- Boards],

    ok = board:disconnect(C0Board),
    ok = board:disconnect(T0Board),
    ok = board:disconnect(S0Board),
    ok = board:disconnect(S1Board),

    ok.


board_init(Config) ->
    C0Board = get_board(circle0, ?config(boards, Config)),
    check_result(board:exec(C0Board, start_hw),
     fun (Expect) ->
        Expect = string:find(Expect, "-circle"),
        ok
     end),
    check_result(board:exec(C0Board, stop_hw),
     fun (Expect) ->
        Expect = string:find(Expect, "-circle"),
        ok
     end),

    ok.


leds_on(Config) ->
    C0Board = get_board(circle0, ?config(boards, Config)),
    check_result(board:exec(C0Board, leds, "-blink"),
     fun (Expect) ->
        Expect = string:find(Expect, "-circle -blink"),
        ok
     end),
    check_result(board:exec(C0Board, leds, "-off"),
     fun (Expect) ->
        Expect = string:find(Expect, "-circle -off"),
        ok
     end),

    ok.

%%
%% Helpers function
%%

%% start all boards and returns their pid
boards_start(_Config) ->
    BoardsAndConfigs = [
        {circle0, ct:get_config(circle0_config)},
        {triangle0, ct:get_config(triangle0_config)},
        {square0, ct:get_config(square0_config)},
        {square1, ct:get_config(square1_config)}],

    [ begin
        Board = board:start(Name, BoardConfig),
        {Name, Board}
      end || {Name, BoardConfig} <- BoardsAndConfigs ].

%% stop all boards
boards_stop(Config) ->
    Boards = ?config(boards, Config),
    [ ok = board:disconnect(Board) || {_Name, Board} <- Boards ].

%% get board pid from board name
get_board(Name, Boards) ->
    case proplists:get_value(Name, Boards) of
        undefined -> throw("board config undefined");
        Config -> Config
    end.

%% check results using the Expect fun
check_result(ok, _) ->
    ok;

check_result({ok, Result}, Expect) ->
    case catch Expect(Result) of
        ok -> ok;
        _ ->
            throw("unexpected results found")
    end.

