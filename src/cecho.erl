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
	 scrollok/1]).

%% =============================================================================
%% Application API
%% =============================================================================
refresh() ->
    cmd_call(?REFRESH).

cbreak() -> 
    cmd_call(?CBREAK).

nocbreak() -> 
    cmd_call(?NOCBREAK).

echo() -> 
    cmd_call(?ECHO).

noecho() -> 
    cmd_call(?NOECHO).

addch(Char) when is_integer(Char) andalso Char >= 0 andalso Char =< 255 ->
    cmd_call(?ADDCH, Char).

addstr(String) when is_list(String) ->
    Str = lists:flatten(String),
    cmd_call(?ADDSTR, {erlang:iolist_size(Str), Str}).

move(Y, X) when is_integer(X) andalso is_integer(Y) ->
    cmd_call(?MOVE, {Y, X}).

getyx() ->
    cmd_call(?GETYX).

getmaxyx() ->
    cmd_call(?GETMAXYX).

curs_set(Flag) when is_integer(Flag) ->
    cmd_call(?CURS_SET, Flag).

erase() ->
    cmd_call(?ERASE).

has_colors() ->
    cmd_call(?HAS_COLORS).

start_color() ->
    cmd_call(?START_COLOR).

init_pair(N, FColor, BColor) when is_integer(N) andalso is_integer(FColor)
				  andalso is_integer(BColor) ->
    cmd_call(?INIT_PAIR, {N, FColor, BColor}).

attron(Mask) when is_integer(Mask) ->
    cmd_call(?ATTRON, Mask).

attroff(Mask) when is_integer(Mask) ->
    cmd_call(?ATTROFF, Mask).

nl() ->
    cmd_call(?NL).

nonl() ->
    cmd_call(?NONL).

scrollok(BFlag) when is_boolean(BFlag) ->
    cmd_call(?SCROLLOK, BFlag).

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
cmd_call(Cmd) ->
    cmd_call(Cmd, []).

cmd_call(Cmd, Args) ->
    cecho_srv:call(Cmd, Args).
