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
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @author Marina Westman Lönne <malotte@malotte.net>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Marinco search light controller
%%% @end
%%% Created : 18 Oct 2015 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(marinco_spl_srv).

-behaviour(gen_server).

%% API
-export([start_link/0,start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% button api
-export([press/1, release/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_RETRY_INTERVAL, 2000).
-define(DEFAULT_BAUDRATE, 9600).

-define(RIGHT,   $r).
-define(LEFT,    $l).
-define(UP,      $u).
-define(DOWN,    $d).
-define(SOS,     $x).
-define(ONOFF,   $1).
-define(SWEEP,   $w).
-define(SPEED,   $p).
-define(RELEASE, $z).

-record(s, {
	  uart,            %% serial line port id
	  device,          %% device name | simulated | none
	  baud_rate,       %% baud rate to uart
	  retry_interval,  %% Timeout for open retry
	  retry_timer,     %% Timer reference for retry
	  buf = <<>>
	 }).

%%%===================================================================
%%% API
%%%===================================================================

press(Key) when is_atom(Key) ->
    gen_server:cast(?SERVER, {send, Key}).

release(_Key) when is_atom(_Key) ->
    gen_server:cast(?SERVER, {send, release}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Opts0) ->
    lager:debug("opts ~p",[Opts0]),
    Opts = Opts0 ++ application:get_all_env(marinco_spl),
    RetryInterval = proplists:get_value(retry_interval,Opts,
					?DEFAULT_RETRY_INTERVAL),
    Device = case proplists:get_value(device, Opts) of
		 undefined -> os:getenv("MARINCO_SPL_DEVICE");
		 D -> D
	     end,
    Baud = case proplists:get_value(baud, Opts) of
	       undefined ->
		   case os:getenv("MARINCO_SPL_SPEED") of
		       false -> ?DEFAULT_BAUDRATE;
		       ""    -> ?DEFAULT_BAUDRATE;
		       Baud0 -> list_to_integer(Baud0)
		   end;
	       Baud1 -> Baud1
	   end,
    S = #s{ device = Device,
	    baud_rate = Baud,
	    retry_interval = RetryInterval
	  },
    if Device =:= false; Device =:= "" ->
	    lager:error("marinco_spl: missing device argument"),
	    {stop, einval};
       true ->
	    lager:info("marinco_spl: using device ~s@~w\n", 
		  [Device, Baud]),
	    case open(S) of
		{ok, S1} -> {ok, S1};
		Error -> {stop, Error}
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({send,Key}, _From, State) ->
    Reply = send_code(Key, State),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({send,Key}, State) ->
    send_code(Key, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info({uart,U,Data}, S) when S#s.uart =:= U ->
    S1 = handle_input(<<(S#s.buf)/binary, Data/binary>>, S),
    uart:setopt(U, active, once),
    {noreply, S1};

handle_info({uart_error,U,Reason}, S) when U =:= S#s.uart ->
    if Reason =:= enxio ->
	    lager:error("marinco_spl: uart error ~p device ~s unplugged?", 
			[Reason,S#s.device]),
	    {noreply, reopen(S)};
       true ->
	    lager:error("marinco_spl: uart error ~p for device ~s", 
			[Reason,S#s.device]),
	    {noreply, S}
    end;

handle_info({uart_closed,U}, S) when U =:= S#s.uart ->
    lager:error("marinco_spl: uart device closed, will try again in ~p msecs.",
		[S#s.retry_interval]),
    S1 = reopen(S),
    {noreply, S1};

handle_info({timeout,TRef,reopen},S) when TRef =:= S#s.retry_timer ->
    case open(S#s { retry_timer = undefined }) of
	{ok, S1} ->
	    {noreply, S1};
	Error ->
	    {stop, Error, S}
    end;

handle_info(_Info, S) ->
    lager:debug("marinco_spl: got info ~p", [_Info]),
    {noreply, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_code(Key, #s { device = simulated }) ->
    lager:info("sending ~w.",[Key]);
send_code(_Key, #s { device = none }) ->
    ok;
send_code(Key, #s { uart = U }) when U =/= undefined ->
    case Key of
	right -> uart:send(U, ?RIGHT);
	left  -> uart:send(U, ?LEFT);
	up    -> uart:send(U, ?UP);
	down  -> uart:send(U, ?DOWN);
	sos   -> uart:send(U, ?SOS);
	onoff -> uart:send(U, ?ONOFF);
	speed -> uart:send(U, ?SPEED);
	release ->  uart:send(U, ?RELEASE);
	_ -> error
    end.

handle_input(Buffer, S) ->
    lager:info("handle_input: got ~p", [Buffer]),
    S#s { buf = <<>> }.

open(S0=#s {device = simulated }) ->
    lager:debug("marinco_spl: simulated"),
    {ok, S0};
open(S0=#s {device = none }) ->
    lager:debug("marinco_spl: off"),
    {ok, S0};
open(S0=#s {device = DeviceName, baud_rate = Baud }) ->
    UartOpts = [{mode,binary}, {baud, Baud}, {packet, 0},
		{csize, 8}, {stopb,1}, {parity,none}, {active, once}],
    case uart:open(DeviceName, UartOpts) of
	{ok,Uart} ->
	    lager:debug("marinco_spl:open: ~s@~w", [DeviceName,Baud]),
	    {ok, S0#s { uart = Uart }};
	{error,E} when E =:= eaccess; E =:= enoent ->
	    lager:debug("marinco_spl:open: ~s@~w  error ~w, will try again "
		   "in ~p msecs.", [DeviceName,Baud,E,S0#s.retry_interval]),
	    {ok, reopen(S0)};
	Error ->
	    lager:error("marinco_spl: error ~w", [Error]),
	    Error
    end.

reopen(S) ->
    if S#s.uart =/= undefined ->
	    lager:debug("marinco_spl: closing device ~s", [S#s.device]),
	    R = uart:close(S#s.uart),
	    lager:debug("marinco_spl: closed ~p", [R]),
	    R;
       true ->
	    ok
    end,
    Timer = start_timer(S#s.retry_interval, reopen),
    S#s { uart=undefined, buf=(<<>>), retry_timer=Timer }.

start_timer(undefined, _Tag) ->
    undefined;
start_timer(infinity, _Tag) ->
    undefined;
start_timer(Time, Tag) ->
    erlang:start_timer(Time,self(),Tag).
