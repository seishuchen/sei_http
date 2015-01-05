%% Feel free to use, reuse and abuse the code in this file.

%% @doc sei_test handler.
-module(sei_test_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("../include/sei_http.hrl").

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    io:format("##########Req:~p ~n",[Req]),
    %ReqPath = sei_http_request:path(Req),
    %io:format("#######ReqPath:~p ~n",[ReqPath]),
    ReqQs = sei_http_request:qs(Req),
    io:format("#######ReqQs:~p ~n",[ReqQs]),
    ReqArgs = sei_http_request:split_qs(ReqQs),
    io:format("#######ReqArgs:~p ~n",[ReqArgs]),
   % Body = <<"Hello,This is sei_test_handler!">>,
   % {ok, Req2} = text_reply(Body,Req),
    T6={struct, [{hello,asdf},{from,1},{to, {struct, [{a,aa}]}} ]},
    JsonBody = iolist_to_binary(sei_json:encode(T6)),
    {ok, Req2} = json_reply(JsonBody,Req),
    %io:format("##########Req2:~p ~n",[Req2]),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.


%% ====================================================================
%% Internal functions
%% ====================================================================

text_reply(Body, Req) when is_list(Body) ->
    text_reply(list_to_binary(Body),Req);
text_reply(Body, Req) when is_binary(Body) ->
    cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], Body, Req);
text_reply(_Body, Req) ->
    ErrReason = <<"error: reponse type is not text/plain!">>,
    error_reply(ErrReason,Req). 

json_reply(Body, Req) when is_list(Body) ->
    json_reply(iolist_to_binary(Body),Req);
json_reply(Body, Req) when is_binary(Body) ->
    cowboy_req:reply(200, [
        {<<"content-type">>, <<"application/json">>}
    ], Body, Req);
json_reply(_Body, Req) ->
    ErrReason = <<"error: reponse type is not application/json">>,
    error_reply(ErrReason,Req). 
    
error_reply(ErrReason, Req) ->
    cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], ErrReason, Req).
    
