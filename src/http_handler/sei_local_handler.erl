%% Feel free to use, reuse and abuse the code in this file.

%% @doc siei_local handler.
-module(sei_local_handler).

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
	], <<"local">>, Req),
    io:format("##########Req2:~p ~n",[Req2]),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
