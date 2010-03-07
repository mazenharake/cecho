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

-module(cecho_srv).
-behaviour(gen_server).
-include("include/cecho.hrl").
-include("include/cecho_commands.hrl").

%% Behaviour Callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3]).

%% Module API
-export([start_link/0, call/2 ]).

%% Records
-record(state, { port }).

%% =============================================================================
%% Module API
%% =============================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

call(Cmd, Args) ->
    gen_server:call(?MODULE, {call, Cmd, Args}, infinity).

%% =============================================================================
%% Behaviour Callbacks
%% =============================================================================
init(no_args) ->
    process_flag(trap_exit, true),
    case erl_ddll:load(code:priv_dir(cecho)++"/lib","cecho") of
	ok ->
	    Port = erlang:open_port({spawn, "cecho"}, []),
	    ok = do_call(Port, ?INITSCR),
	    ok = do_call(Port, ?ERASE),
	    {ok, #state{ port = Port }};
	{error, ErrorCode} ->
	    exit({driver_error, erl_ddll:format_error(ErrorCode)})
    end.

handle_call({call, Cmd, Args}, _From, State) ->
    Response = do_call(State#state.port, Cmd, Args),
    {reply, Response, State}.

terminate(_Reason, State) ->
    do_call(State#state.port, ?ENDWIN),
    do_call(State#state.port, ?CURS_SET, ?ceCURS_NORMAL),
    erlang:port_close(State#state.port),
    erl_ddll:unload("cecho").

%% @hidden
handle_cast(_, State) ->
    {noreply, State}.

%% @hidden
handle_info(_, State) ->
    {noreply, State}.

%% @hidden
code_change(_, State, _) ->
    {noreply, State}.

%% =============================================================================
%% Internal Functions
%% =============================================================================
do_call(Port, Cmd) ->
    do_call(Port, Cmd, []).

do_call(Port, Cmd, Args) ->
    binary_to_term(erlang:port_control(Port, Cmd, term_to_binary(Args))).
