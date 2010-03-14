%% Copyright (c) 2010, Mazen Harake
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%% 
%%  * Redistributions of source code must retain the above copyright notice,
%%    this list of conditions and the following disclaimer.
%%  * Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%  * Neither the name of the <ORGANIZATION> nor the names of its
%%    contributors may be used to endorse or promote products derived from
%%    this software without specific prior written permission.
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
%% POSSIBILITY OF SUCH DAMAGE.x 

-define(ceTRUE, 1).
-define(ceFALSE, 0).

-define(ceCURS_INVISIBLE, 0).
-define(ceCURS_NORMAL, 1).
-define(ceCURS_VERY_VISIBLE, 2).

-define(ceCOLOR_BLACK, 0).
-define(ceCOLOR_RED, 1).
-define(ceCOLOR_GREEN, 2).
-define(ceCOLOR_YELLOW, 3).
-define(ceCOLOR_BLUE, 4).
-define(ceCOLOR_MAGENTA, 5).
-define(ceCOLOR_CYAN, 6).
-define(ceCOLOR_WHITE, 7).

-define(ceA_NORMAL, 0).
-define(ceCOLOR_PAIR(C), (C bsl 8)).
-define(ceA_BOLD, (1 bsl (8 + 13))).
-define(ceA_UNDERLINE, (1 bsl (8 + 9))).
-define(ceA_REVERSE, (1 bsl (8 + 10))).
-define(ceA_BLINK, (1 bsl (8 + 11))).

-define(ceSTDSCR, 0).
