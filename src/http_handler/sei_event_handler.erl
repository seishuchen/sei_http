%%%-------------------------------------------------------------------
%%% @author seishuchen
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jan 2015 8:53 AM
%%%-------------------------------------------------------------------
-module(sei_event_handler).
-author("seishuchen").

%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("../include/sei_http.hrl").

init(_Type, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  io:format("##########Req:~p ~n",[Req]),
  %ReqCmd = gb_http_request:path(Req),
  %io:format("#######ReqCmd:~p ~n",[ReqCmd]),
  {ok, Req2} = cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/plain">>}
  ], <<"event">>, Req),
  io:format("##########Req2:~p ~n",[Req2]),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.