%% Copyright (c) 2010, Mazen Harake
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%     * Redistributions of source code must retain the above copyright notice,
%%       this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of the <ORGANIZATION> nor the names of its
%%       contributors may be used to endorse or promote products derived from
%%       this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

-module(cecho).
-behaviour(application).
-include("include/cecho.hrl").
-include("include/cecho_commands.hrl").

%% Behaviour Callbacks
-export([start/2, stop/1]).

%% Application API
-export([refresh/0, cbreak/0, nocbreak/0, echo/0, noecho/0, addch/1, addstr/1,
	 move/2, getyx/0, getmaxyx/0, curs_set/1, erase/0, has_colors/0,
	 start_color/0, init_pair/3, attron/1, attroff/1, nl/0, nonl/0,
	 scrollok/2, mvaddch/3, mvaddstr/3, newwin/4, delwin/1, wmove/3, 
	 waddstr/2, waddch/2, mvwaddstr/4, mvwaddch/4, wrefresh/1]).

%% =============================================================================
%% Application API
%% =============================================================================
refresh() ->
    call(?REFRESH).

cbreak() -> 
    call(?CBREAK).

nocbreak() -> 
    call(?NOCBREAK).

echo() -> 
    call(?ECHO).

noecho() -> 
    call(?NOECHO).

addch(Char) when is_integer(Char) andalso Char >= 0 andalso Char =< 255 ->
    call(?ADDCH, Char).

addstr(String) when is_list(String) ->
    Str = lists:flatten(String),
    call(?ADDSTR, {erlang:iolist_size(Str), Str}).

move(Y, X) when is_integer(X) andalso is_integer(Y) ->
    call(?MOVE, {Y, X}).

getyx() ->
    call(?GETYX).

getmaxyx() ->
    call(?GETMAXYX).

curs_set(Flag) when is_integer(Flag) ->
    call(?CURS_SET, Flag).

erase() ->
    call(?ERASE).

has_colors() ->
    call(?HAS_COLORS).

start_color() ->
    call(?START_COLOR).

init_pair(N, FColor, BColor) when is_integer(N) andalso is_integer(FColor)
				  andalso is_integer(BColor) ->
    call(?INIT_PAIR, {N, FColor, BColor}).

attron(Mask) when is_integer(Mask) ->
    call(?ATTRON, Mask).

attroff(Mask) when is_integer(Mask) ->
    call(?ATTROFF, Mask).

nl() ->
    call(?NL).

nonl() ->
    call(?NONL).

scrollok(Window, BFlag) when is_integer(Window) andalso is_boolean(BFlag) ->
    call(?SCROLLOK, {Window, BFlag}).

mvaddch(Y, X, Char) when is_integer(Char) andalso Char >= 0 andalso Char =< 255
                         andalso is_integer(X) andalso is_integer(Y) ->
    call(?MVADDCH, {Y, X, Char}).

mvaddstr(Y, X, String) when is_list(String) andalso is_integer(X) andalso 
			    is_integer(Y) ->
    Str = lists:flatten(String),
    call(?MVADDSTR, {Y, X, erlang:iolist_size(Str), Str}).

newwin(Height, Width, StartY, StartX) when is_integer(Height) andalso 
					   is_integer(Width) andalso 
					   is_integer(StartY) andalso
					   is_integer(StartX) ->
    call(?NEWWIN, {Height, Width, StartY, StartX}).

delwin(Window) when is_integer(Window) ->
    call(?DELWIN, Window).

wmove(Window, Y, X) when is_integer(Window) andalso is_integer(Y) andalso 
			 is_integer(X) ->
    call(?WMOVE, {Window, Y, X}).

waddstr(Window, String) when is_integer(Window) andalso is_list(String) ->
    Str = lists:flatten(String),
    call(?WADDSTR, {Window, erlang:iolist_size(Str), Str}).

waddch(Window, Char) when is_integer(Window) andalso is_integer(Char) andalso 
                          Char >= 0 andalso Char =< 255  ->
    call(?WADDCH, {Window, Char}).

mvwaddstr(Window, Y, X, String) when is_integer(Window) andalso is_integer(Y)
				     andalso is_integer(X) andalso 
				     is_list(String) ->
    Str = lists:flatten(String),
    call(?MVWADDSTR, {Window, Y, X, erlang:iolist_size(Str), Str}).

mvwaddch(Window, Y, X, Char) when is_integer(Window) andalso is_integer(Y)
                                  andalso is_integer(X) andalso 
                                  Char >= 0 andalso Char =< 255  ->
    call(?MVWADDCH, {Window, Y, X, Char}).

wrefresh(Window) when is_integer(Window) ->
    call(?WREFRESH, Window).
    
%% =============================================================================
%% Behaviour Callbacks
%% =============================================================================
start(_, _) ->
    cecho_srv:start_link().

stop(_) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================
call(Cmd) ->
    call(Cmd, []).

call(Cmd, Args) ->
    cecho_srv:call(Cmd, Args).
