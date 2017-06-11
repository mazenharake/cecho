%%==============================================================================
%% Copyright (c) 2017, Mazen Harake
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
%%==============================================================================

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

-define(ceACS_DIAMOND, 4194400).
-define(ceACS_CKBOARD, 4194401).
-define(ceACS_DEGREE, 4194406).
-define(ceACS_PLMINUS, 4194407).
-define(ceACS_BOARD, 4194408).
-define(ceACS_LANTERN, 4194409).
-define(ceACS_LRCORNER, 4194410).
-define(ceACS_URCORNER, 4194411).
-define(ceACS_ULCORNER, 4194412).
-define(ceACS_LLCORNER, 4194413).
-define(ceACS_PLUS, 4194414).
-define(ceACS_S1, 4194415).
-define(ceACS_S3, 4194416).
-define(ceACS_HLINE, 4194417).
-define(ceACS_S7, 4194418).
-define(ceACS_S9, 4194419).
-define(ceACS_LTEE, 4194420).
-define(ceACS_RTEE, 4194421).
-define(ceACS_BTEE, 4194422).
-define(ceACS_TTEE, 4194423).
-define(ceACS_VLINE, 4194424).
-define(ceACS_LEQUAL, 4194425).
-define(ceACS_GEQUAL, 4194426).
-define(ceACS_PI, 4194427).
-define(ceACS_NEQUAL, 4194428).
-define(ceACS_STERLING, 4194429).
-define(ceACS_BULLET, 4194430).
-define(ceACS_RARROW, 4194347).
-define(ceACS_LARROW, 4194348).
-define(ceACS_UARROW, 4194349).
-define(ceACS_DARROW, 4194350).
-define(ceACS_BLOCK, 4194352).

-define(ceKEY_TAB, 9).
-define(ceKEY_ESC, 27).


-define(ceKEY_DOWN, 258).
-define(ceKEY_UP, 259).
-define(ceKEY_LEFT, 260).
-define(ceKEY_RIGHT, 261).
-define(ceKEY_HOME, 262).
-define(ceKEY_F(N), 264+N).
-define(ceKEY_DEL, 330).
-define(ceKEY_INS, 331).
-define(ceKEY_PGDOWN, 338).
-define(ceKEY_PGUP, 339).
-define(ceKEY_END, 360).


