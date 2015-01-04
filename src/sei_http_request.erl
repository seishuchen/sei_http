-module(sei_http_request).
-include("../include/sei_http.hrl").

-export([send_chunk/2
         , async_send_chunk/2
         , chunk_ref/1
         , close_chunk/1
         , path/1
         , path_info/1
         , qs/1
         , qs_vals/1
         , split_qs/1
         , get_header/2
         , headers/1
         , peer/1
         , method/1
         , get_header/3
         , to_proplist/1
         , is_request/1
        ]).

%%
%% Helpers for working with a #http_req{}
%%


%% @doc: Returns path split into binary parts.
path(#http_req{path = Path})          -> Path.
path_info(#http_req{path_info = PathInfo})  -> PathInfo.
qs(#http_req{qs = Qs})          -> Qs.
qs_vals(#http_req{qs_vals = QsVals})  -> QsVals.
headers(#http_req{headers = Headers}) -> Headers.
method(#http_req{method = Method})    -> Method.
peer(#http_req{peer = Peer}) -> Peer.

get_header(Key, #http_req{headers = Headers}) ->
    proplists:get_value(Key, Headers).

get_header(Key, #http_req{headers = Headers}, Default) ->
    proplists:get_value(Key, Headers, Default).

%% @doc: Splits the url arguments into a proplist. Lifted from
%% cowboy_http:x_www_form_urlencoded/2
-spec split_qs(binary()) -> list({binary(), binary() | true}).
split_qs(Qs) ->
    split_args(Qs).

-spec split_args(binary()) -> list({binary(), binary() | true}).
split_args(<<>>) ->
	[];
split_args(Qs) ->
	Tokens = binary:split(Qs, <<"&">>, [global, trim]),
	[case binary:split(Token, <<"=">>) of
		[Token] -> {Token, true};
		[Name, Value] -> {Name, Value}
	end || Token <- Tokens].

parse_path({abs_path, FullPath}) ->
    case binary:split(FullPath, [<<"?">>]) of
        [URL]       -> {ok, {FullPath, split_path(URL), []}};
        [URL, Args] -> {ok, {FullPath, split_path(URL), split_args(Args)}}
    end;
parse_path({absoluteURI, _Scheme, _Host, _Port, Path}) ->
    parse_path({abs_path, Path});
parse_path(_) ->
    {error, unsupported_uri}.

split_path(Path) ->
    [P || P <- binary:split(Path, [<<"/">>], [global]),
          P =/= <<>>].

remove_whitespace(Bin) ->
    binary:replace(Bin,<<" ">>, <<>>, [global]).

%% @doc: Serializes the request record to a proplist. Useful for
%% logging
to_proplist(#http_req{} = Req) ->
    lists:zip(record_info(fields, http_req), tl(tuple_to_list(Req))).



%% @doc: Returns a reference that can be used to send chunks to the
%% client. If the protocol does not support it, returns {error,
%% not_supported}.
chunk_ref(#http_req{version = {1, 1}} = Req) ->
    Req#http_req.pid;
chunk_ref(#http_req{}) ->
    {error, not_supported}.


%% @doc: Explicitly close the chunked connection. Returns {error,
%% closed} if the client already closed the connection.
close_chunk(Ref) ->
    send_chunk(Ref, <<"">>).

%% @doc: Sends a chunk asynchronously
async_send_chunk(Ref, Data) ->
    Ref ! {chunk, Data}.

%% @doc: Sends a chunk synchronously, if the refrenced process is dead
%% returns early with {error, closed} instead of timing out.
send_chunk(Ref, Data) ->
    case is_ref_alive(Ref) of
        false -> {error, closed};
        true  -> send_chunk(Ref, Data, 5000)
    end.

send_chunk(Ref, Data, Timeout) ->
    Ref ! {chunk, Data, self()},
    receive
        {Ref, ok} ->
            ok;
        {Ref, {error, Reason}} ->
            {error, Reason}
    after Timeout ->
            {error, timeout}
    end.

is_ref_alive(Ref) ->
    case node(Ref) =:= node() of
        true -> is_process_alive(Ref);
        false -> rpc:call(node(Ref), erlang, is_process_alive, [Ref])
    end.

is_request(#http_req{}) -> true;
is_request(_)      -> false.
