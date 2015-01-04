%% Feel free to use, reuse and abuse the code in this file.

%% @doc sei_divece handler.
-module(sei_stat_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("../include/sei_http.hrl").

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    io:format("##########Req:~p ~n",[Req]),
    %ReqCmd = sei_http_request:path(Req),
    %io:format("#######ReqCmd:~p ~n",[ReqCmd]),
	{ok, Req2} = cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain">>}
	], <<"device!">>, Req),
    io:format("##########Req2:~p ~n",[Req2]),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
