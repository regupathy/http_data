%%%-------------------------------------------------------------------
%%% @author regupathy.b
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2016 1:00 PM
%%%-------------------------------------------------------------------
-module(http_data).
-author("regupathy.b").
-include("http.hrl").

%% API
-export([new_request/0,new_response/0,post/1,get/1,put/1,option/1,head/1,version/2,action/2,headers/2,body/2]).
-export([set_code/2,set_content_type/2,decide_content_type/1]).

-export([post/2,content/3]).
%%%===================================================================
%%% APIs
%%%===================================================================

new_request() -> #http_data{for = request,content_type = #content_type.raw}.

new_response() -> #http_data{for = response,content_type = #content_type.raw}.


post(#http_data{} = H) -> H#http_data{request = #method.post}.

post(Action,Headers) -> #http_data{request = #method.post,for = request,action = Action,headers = Headers,content_type = #content_type.raw}.

get(#http_data{} = H) -> H#http_data{request = #method.get,for = request}.

put(#http_data{} = H) -> H#http_data{request = #method.put,for = request}.

option(#http_data{} = H) -> H#http_data{request = #method.option,for = request}.

head(#http_data{} = H) -> H#http_data{request = #method.head,for = request}.

version(<<Version/binary>>,#http_data{} =H) -> H#http_data{version = Version}.

action(<<$/,Action/binary>>,#http_data{} =H) -> H#http_data{action = Action};

action(<<Action/binary>>,#http_data{} =H) -> H#http_data{action = Action}.

headers(Headers,#http_data{} = HttpState) -> HttpState#http_data{headers = Headers}.

body(Body,#http_data{} = HttpState) -> HttpState#http_data{body = Body}.

content(#content_type.json,Body,#http_data{} =H) -> {ok,JSONBIN} = json_encoder:build(Body),
  H#http_data{content_type = #content_type.json,body = JSONBIN};
content(ContentType,Body,#http_data{} =H) -> H#http_data{content_type = ContentType,body = Body}.

set_code(<<Code/binary>>,#http_data{} = HttpState) -> HttpState#http_data{code = code(Code),for = response};

set_code(Code,#http_data{} = HttpState)when is_number(Code) -> HttpState#http_data{code = Code,for=response}.

set_content_type(#content_type.html,#http_data{} = HttpState) -> HttpState#http_data{content_type = #content_type.html};
set_content_type(#content_type.json,#http_data{} = HttpState) -> HttpState#http_data{content_type = #content_type.json};
set_content_type(#content_type.text,#http_data{} = HttpState) -> HttpState#http_data{content_type = #content_type.text};
set_content_type(#content_type.xml,#http_data{} = HttpState) -> HttpState#http_data{content_type = #content_type.xml};
set_content_type(_,#http_data{} = HttpState) -> HttpState#http_data{content_type = #content_type.raw}.

decide_content_type(<<"application/json">>) -> #content_type.json;
decide_content_type(<<"application/json",_/binary>>) -> #content_type.json;
decide_content_type(<<"application/xml">>) -> #content_type.xml;
decide_content_type(<<"text/plain">>) -> #content_type.text;
decide_content_type(<<"application/html">>) -> #content_type.html;
decide_content_type(_) -> #content_type.raw.

%%%===================================================================
%%%                         Internal Functions
%%%===================================================================

code(<<"200">>) -> #http_response_code.ok;
code(<<"202">>) -> #http_response_code.accepted;
code(<<"101">>) -> #http_response_code.websocket_accepted;
code(<<"400">>) -> #http_response_code.bad_request;
code(<<"403">>) -> #http_response_code.forbidden;
code(<<"404">>) -> #http_response_code.not_found;
code(<<"500">>) -> #http_response_code.internal_server_error;
code(<<"503">>) -> #http_response_code.service_unavailable;
code(<<"550">>) -> #http_response_code.permission_denied;
code(ResponseCode) -> io:format("Unhandle HTTP Response Code :~p",[ResponseCode]), throw(undefined_http_response_code).



