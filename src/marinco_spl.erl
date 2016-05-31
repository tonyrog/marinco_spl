%%% coding: latin-1
%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2015, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @author Marina Westman Lönne <malotte@malotte.net>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Marinco search light controller
%%% @end
%%% Created : 19 Oct 2015 by Tony Rogvall <tony@rogvall.se>

-module(marinco_spl).

-export([start/0]).
%% button api
-export([press/1, release/1]).

%% test api
-export([pause/0, resume/0, ifstatus/0]).

-define(SERVER, marinco_spl_srv).

start() ->
    application:start(lager),
    application:start(uart),
    application:start(marinco_spl).


press(Key) when is_atom(Key) ->
    ?SERVER:press(Key).

release(Key) when is_atom(Key) ->
    ?SERVER:release(Key).

-spec pause() -> ok | {error, Error::atom()}.
pause() ->
    ?SERVER:pause().

-spec resume() -> ok | {error, Error::atom()}.
resume() ->
    ?SERVER:resume().

-spec ifstatus() -> {ok, Status::atom()} | {error, Error::atom()}.
ifstatus() ->
    ?SERVER:ifstatus().

