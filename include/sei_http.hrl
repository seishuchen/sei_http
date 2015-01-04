-type http_headers() :: [{binary(), iodata()}].

-type http_status() :: non_neg_integer() | binary().

-type http_version() :: 'HTTP/1.1' | 'HTTP/1.0'.

-type http_method() :: 'OPTIONS' | 'GET' | 'HEAD' | 'POST' |
                       'PUT' | 'DELETE' | 'TRACE' | binary().
                       %% binary for other http methods

-type onrequest_fun() :: fun((Req) -> Req).

-type onresponse_fun() ::
	fun((http_status(), http_headers(), iodata(), Req) -> Req).

-type content_decode_fun() :: fun((binary())
	-> {ok, binary()}
	| {error, atom()}).
-type transfer_decode_fun() :: fun((binary(), any())
	-> {ok, binary(), binary(), any()}
	| more | {more, non_neg_integer(), binary(), any()}
	| {done, non_neg_integer(), binary()}
	| {done, binary(), non_neg_integer(), binary()}
	| {error, atom()}).

-type resp_body_fun() :: fun((any(), module()) -> ok).
-type send_chunk_fun() :: fun((iodata()) -> ok | {error, atom()}).
-type resp_chunked_fun() :: fun((send_chunk_fun()) -> ok).
-type bindings() :: [{atom(), binary()}].
-type tokens() :: [binary()].
-type cookie_option() :: {max_age, non_neg_integer()}
    | {domain, binary()} | {path, binary()}
    | {secure, boolean()} | {http_only, boolean()}.
-type cookie_opts() :: [cookie_option()].


-record(http_req, {
	%% Transport.
	socket = undefined :: any(),
	transport = undefined :: undefined | module(),
	connection = keepalive :: keepalive | close,

	%% Request.
	pid = undefined :: pid(),
	method = <<"GET">> :: binary(),
	version = 'HTTP/1.1' :: http_version(),
	peer = undefined :: undefined | {inet:ip_address(), inet:port_number()},
	host = undefined :: undefined | binary(),
	host_info = undefined :: undefined | tokens(),
	port = undefined :: undefined | inet:port_number(),
	path = undefined :: binary(),
	path_info = undefined :: undefined | tokens(),
	qs = undefined :: binary(),
	qs_vals = undefined :: undefined | list({binary(), binary() | true}),
	bindings = undefined :: undefined | bindings(),
	headers = [] :: http_headers(),
	p_headers = [] :: [any()], %% @todo Improve those specs.
	cookies = undefined :: undefined | [{binary(), binary()}],
	meta = [] :: [{atom(), any()}],

	%% Request body.
	body_state = waiting :: waiting | done | {stream, non_neg_integer(),
		transfer_decode_fun(), any(), content_decode_fun()},
	multipart = undefined :: undefined | {non_neg_integer(), fun()},
	buffer = <<>> :: binary(),

	%% Response.
	resp_compress = false :: boolean(),
	resp_state = waiting :: locked | waiting | waiting_stream
		| chunks | stream | done,
	resp_headers = [] :: http_headers(),
	resp_body = <<>> :: iodata() | resp_body_fun()
		| {non_neg_integer(), resp_body_fun()}
		| {chunked, resp_chunked_fun()},

	%% Functions.
	onresponse = undefined :: undefined | already_called
		| onresponse_fun()
}).


-define(l2i(L), list_to_integer(L)).
-define(i2l(I), integer_to_list(I)).
-define(b2i(I), list_to_integer(binary_to_list(I))).

