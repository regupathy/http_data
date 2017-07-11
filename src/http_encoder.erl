%%%-------------------------------------------------------------------
%%% @author regupathy.b
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2016 11:37 AM
%%%-------------------------------------------------------------------
-module(http_encoder).
-author("regupathy.b").
-include("http.hrl").

%% API
-export([build/1]).

%%%===================================================================
%%% APIs
%%%===================================================================

-spec build(DataSet::#http_data{}) -> {ok,Data::binary()} | {wrong_data,Reason::any()}.

build(#http_data{for = undefined} = _) -> {wrong_data,<<" Not a proper HTTP data ">>};

build(#http_data{for = request} = H) -> request(H);

build(#http_data{for = response} = H) -> response(H);

build(_) -> {wrong_data,<<"Invalid data">>}.

%%%===================================================================
%%% Intenal Methods
%%%===================================================================

response(#http_data{code = undefined} = _) -> {wrong_data,<<"Response code is missing">>};
response(#http_data{code = Code,version = Version} = H) -> headers(H,binary_util:merge([<<Version/binary,32>>,code_value(Code),<<$\r,$\n>>])).

request(#http_data{request = undefined} = _ ) -> {wrong_data,<<"Request Method is missing">>};
request(#http_data{request = Method,action = Action,version = Version} = H)-> headers(H,binary_util:merge([method(Method),$ ,$/,Action,$ ,Version,$\r,$\n])).

headers(#http_data{headers = Headers} = H,Bin) -> headers(Headers,H,Bin).
%% headers([],#http_data{content_type = undefined} =_,_Bin) -> {wrong_data,<<"Content Type is missing">>};
headers([],#http_data{content_type = ContentType,body = Content} = _,Bin) -> body(Content,predefine_headers(size(Content),ContentType,Bin));
headers([{<<"Content-Length">>,_Length}|T],H,Bin) -> headers(T,H,Bin);
headers([{<<Key/binary>>,<<Val/binary>>}|T],H,Bin) -> headers(T,H,header(Key,Val,Bin)).

predefine_headers(Length,#content_type.raw,Bin) -> header(<<"Content-Length">>,integer_to_binary(Length),Bin);
predefine_headers(Length,CType,Bin) -> header(<<"Content-Type">>,content_type(CType),header(<<"Content-Length">>,integer_to_binary(Length),Bin)).

header(Key,Val,Bin)-> <<Bin/binary,Key/binary,$:,Val/binary,$\r,$\n>>.

body(<<>>,Bin) -> {ok,<<Bin/binary,13,10>>};
body(<<Content/binary>>,Bin) -> {ok,<<Bin/binary,13,10,Content/binary>>}.

%%%===================================================================
%%% Helper mehtods
%%%===================================================================

code_value(#http_response_code.ok) -> <<"200",32,"OK">>;

code_value(#http_response_code.accepted) -> <<"202",32,"Accepted">>;

code_value(#http_response_code.websocket_accepted) -> <<"101",32,"Switching Protocol">>;

code_value(#http_response_code.bad_request) -> <<"400",32,"Bad Request">>;

code_value(#http_response_code.forbidden) -> <<"403",32,"Forbidden">>;

code_value(#http_response_code.not_found) -> <<"404",32,"Not Found">>;

code_value(#http_response_code.internal_server_error) -> <<"500",32,"Internal Server Error">>;

code_value(#http_response_code.service_unavailable) -> <<"503",32,"Service Unavailable">>;

code_value(#http_response_code.permission_denied) -> <<"550",32,"Permission denied">>.

method(#method.put) -> <<"PUT">>;
method(#method.get) -> <<"GET">>;
method(#method.post) -> <<"POST">>;
method(#method.head) -> <<"HEAD">>;
method(#method.option) -> <<"OPTION">>.

content_type(#content_type.xml) -> <<"application/xml">>;
content_type(#content_type.json) -> <<"application/json">>;
content_type(#content_type.text) -> <<"text/plain">>;
content_type(#content_type.html) -> <<"application/html">>.



