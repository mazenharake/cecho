-module(cecho_example).
-compile(export_all).

-include("cecho.hrl").

%%
%% Simple countdown which shows how to print, move and get coordinates
%%
countdown(S) ->
    application:start(cecho),
    cecho:cbreak(),
    cecho:noecho(),
    cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:move(1, 1),
    Flag = cecho:has_colors(),
    cecho:addstr(io_lib:format("Has color: ~p",[Flag])),
    print_colors(Flag),
    cecho:move(10, 10),
    cecho:addstr("Countdown: "),
    cecho:refresh(),
    count_it_down(S),
    cecho:curs_set(?ceCURS_NORMAL),
    timer:sleep(2000),
    application:stop(cecho).

count_it_down(S) when S =< 0 ->
    cecho:move(10, 22),
    cecho:addstr("BOOOOM!"),
    cecho:refresh();
count_it_down(S) ->
    cecho:move(10+S, 22),
    {X, Y} = cecho:getyx(),
    {MX, MY} = cecho:getmaxyx(),
    cecho:addstr(io_lib:format("~p",[S])),
    cecho:move(22,22),
    cecho:addstr(io_lib:format("~p:~p (~p:~p)",[X,Y,MX,MY])),
    cecho:refresh(),
    timer:sleep(1000),
    count_it_down(S-1).

print_colors(false) -> ok;
print_colors(true) ->
    cecho:start_color(),
    cecho:init_pair(1, ?ceCOLOR_RED, ?ceCOLOR_BLACK),
    cecho:attron(?ceA_BOLD bor ?ceCOLOR_PAIR(1)),
    cecho:move(2,1),
    cecho:addstr("Colored!"),
    cecho:refresh(),
    cecho:attroff(?ceA_BOLD bor ?ceCOLOR_PAIR(1)),
    ok.

%%
%% Simple example to show usage
%%
simple() ->
    application:start(cecho),
    ok = cecho:nocbreak(),
    ok = cecho:cbreak(),
    ok = cecho:echo(),
    ok = cecho:noecho(),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    ok = cecho:move(7, 10),
    ok = cecho:addch(45),
    ok = cecho:addch(45),
    ok = cecho:move(7, 12),
    ok = cecho:addstr(" Information ----"),
    ok = cecho:move(8, 10),
    {Row, Col} = cecho:getyx(),
    {MRow, MCol} = cecho:getmaxyx(),
    ok = cecho:addstr(io_lib:format("Row:~p Col:~p MaxRow:~p MaxCol:~p",
				    [Row,Col,MRow,MCol])),
    case cecho:has_colors() of
	true ->
	    cecho:start_color(),
	    ok = cecho:init_pair(1, ?ceCOLOR_BLUE, ?ceCOLOR_WHITE),
	    ok = cecho:init_pair(2, ?ceCOLOR_GREEN, ?ceCOLOR_YELLOW), 
	    cecho:move(9, 10),
	    cecho:attron(?ceCOLOR_PAIR(1) bor ?ceA_BOLD bor ?ceA_UNDERLINE),
	    cecho:addstr(" Has Colors! "),
	    cecho:attron(?ceCOLOR_PAIR(2)),
	    cecho:addstr(" Yes!! "),
	    cecho:attroff(?ceCOLOR_PAIR(1) bor ?ceCOLOR_PAIR(2) bor ?ceA_BOLD bor ?ceA_UNDERLINE);
	false ->
	    cecho:move(9, 10),
	    cecho:addstr(" No colors :( ")
    end,		     
    cecho:addch($!),
    ok = cecho:refresh(),
    timer:sleep(5000),
    cecho:curs_set(?ceCURS_NORMAL),
    application:stop(cecho).

%%
%% Fun example
%%
colors() ->
    application:start(cecho),
    ok = cecho:cbreak(),
    ok = cecho:noecho(),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    ok = cecho:start_color(),
    %% ok = cecho:init_pair(1, ?ceCOLOR_RED, ?ceCOLOR_BLACK),
    %% ok = cecho:init_pair(2, ?ceCOLOR_GREEN, ?ceCOLOR_BLACK),
    %% ok = cecho:init_pair(3, ?ceCOLOR_YELLOW, ?ceCOLOR_BLACK),
    %% ok = cecho:init_pair(4, ?ceCOLOR_BLUE, ?ceCOLOR_BLACK),
    %% ok = cecho:init_pair(5, ?ceCOLOR_MAGENTA, ?ceCOLOR_BLACK),
    %% ok = cecho:init_pair(6, ?ceCOLOR_CYAN, ?ceCOLOR_BLACK),
    %% ok = cecho:init_pair(7, ?ceCOLOR_WHITE, ?ceCOLOR_BLACK),
    %% ok = cecho:init_pair(8, ?ceCOLOR_BLACK, ?ceCOLOR_BLACK),
    ok = cecho:init_pair(1, ?ceCOLOR_BLACK, ?ceCOLOR_RED),
    ok = cecho:init_pair(2, ?ceCOLOR_BLACK, ?ceCOLOR_GREEN),
    ok = cecho:init_pair(3, ?ceCOLOR_BLACK, ?ceCOLOR_YELLOW),
    ok = cecho:init_pair(4, ?ceCOLOR_BLACK, ?ceCOLOR_BLUE),
    ok = cecho:init_pair(5, ?ceCOLOR_BLACK, ?ceCOLOR_MAGENTA),
    ok = cecho:init_pair(6, ?ceCOLOR_BLACK, ?ceCOLOR_CYAN),
    ok = cecho:init_pair(7, ?ceCOLOR_BLACK, ?ceCOLOR_WHITE),
    ok = cecho:init_pair(8, ?ceCOLOR_BLACK, ?ceCOLOR_BLACK),
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    {MaxRow, MaxCol} = cecho:getmaxyx(),
    cecho:move(10,10),
    cecho:addstr(lists:flatten(io_lib:format("Max Row: ~p, Max Col: ~p",[MaxRow, MaxCol]))),
    cecho:move(0, 0),
    cecho:addch($@),
    cecho:move(MaxRow-1, 0),
    cecho:addch($@),
    cecho:move(0, MaxCol-1),
    cecho:addch($@),
    %% -2 on next row because we can't write in last row and col becase we will segfault
    %% don't know why yet :( probably because of a scrolling/newline issue somewhere.
    cecho:move(MaxRow-2, MaxCol-1),
    cecho:addch($@),
    cecho:refresh(),
    timer:sleep(2000),
    do_colors(MaxRow-1, MaxCol, 2000),
    application:stop(cecho).

do_colors(_,_,0) -> ok;
do_colors(MR,MC,N) ->
    ch_colors(MR,MC,1000),
    cecho:refresh(),
    timer:sleep(100),
    do_colors(MR, MC, N-1).

ch_colors(_,_,0) -> ok;
ch_colors(MR, MC, N) ->
    R = random:uniform(MR)-1,
    C = random:uniform(MC)-1,
    CN = random:uniform(8),
    cecho:attron(?ceCOLOR_PAIR(CN)),
    cecho:move(R, C),
    cecho:addch($ ),
    cecho:move(R, C),
    ch_colors(MR, MC, N-1).

%%
%% Simply puts an @ character somewhere. If you go out of bounds you crash
%% 
pos(Y, X) ->
    application:start(cecho),
    ok = cecho:cbreak(),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:move(Y, X),
    cecho:addstr("@"),
    cecho:refresh(),
    timer:sleep(2000),
    application:stop(cecho).

%% 
%% Prints a number continuously as another io thread is waiting for keyinput
%%
input() ->
    application:start(cecho),
    ok = cecho:cbreak(),
    ok = cecho:noecho(),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    ok = cecho:keypad(?ceSTDSCR, true),
    spawn_link(?MODULE, input_counter, [0]),
    cecho:mvaddstr(9, 10, "Enter:    "),
    cecho:refresh(),
    input_reader().

input_reader() ->
    P = cecho:getch(),
    case P of
	113 ->
	    application:stop(cecho);
	_ ->
	    cecho:mvaddstr(9, 17, io_lib:format("~p  ",[P])),
	    cecho:refresh(),
	    input_reader()
    end.

input_counter(N) ->
    cecho:mvaddstr(10, 10, io_lib:format("# ~p",[N])),
    cecho:refresh(),
    timer:sleep(100),
    input_counter(N+1).


%%
%% cursmove - move the '@' around the screen with the arrow keys. 'q' to quit.
%%
cursmove() ->
    application:start(cecho),
    cecho:cbreak(),
    cecho:noecho(),
    cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:mvaddch(10, 10, $@),
    cecho:move(10,10),
    cecho:refresh(),
%    upd_pos(),
    moveloop().

moveloop() ->
    [C] = io:get_chars("",1),
    case C of
	$q -> application:stop(cecho);
	65 -> mv(-1, 0); %% Up
	66 -> mv(1, 0); %% Down
	67 -> mv(0, 1); %% Right
	68 -> mv(0, -1); %% Left
	C -> ok
    end,
    cecho:refresh(),
    moveloop().

mv(OffsetY, OffsetX) ->
    {CY, CX} = cecho:getyx(),
    FinalY = CY+(OffsetY),
    FinalX = CX+(OffsetX),
    cecho:mvaddch(FinalY,FinalX,$@),
    cecho:move(FinalY, FinalX).
