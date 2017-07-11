%%%-------------------------------------------------------------------
%%% @author regupathy.b
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2015 5:54 PM
%%%-------------------------------------------------------------------
-module(http_decoder).
-author("regupathy.b").

-record(state,{content_type = unknown, content_length = unknown,method,header_key,balance_length}).

%% API
-export([scan/2]).

scan(Source,[]) -> http(Source,[],#state{});
scan(Source,DataState) -> resume(Source,DataState).

resume(Source,{DataHolder,[{key,Acc}|State],Config}) -> header_key(Source,Acc,DataHolder,[key|State],Config);
resume(Source,{DataHolder,[{value,Acc}|State],Config}) -> header_val(Source,Acc,DataHolder,[value|State],Config);
resume(Source,{DataHolder,[{header_top,Acc}|State],Config}) -> header_top(Source,Acc,DataHolder,[header_top|State],Config);
resume(Source,{DataHolder,[header|_]=State,Config}) -> header(Source,DataHolder,State,Config);
resume(Source,{DataHolder,[body,http]=State,Config}) -> body(Source,DataHolder,State,Config);
resume(_Source,_) -> {wrong_data,<<"Resume Parser is problem">>}.

http(<<$P,$O,$S,$T,$ ,Rest/binary>>,State,Config) ->  header_top(Rest,[],new_http(post),[header_top|State],Config#state{method = post});
http(<<$G,$E,$T,$ ,Rest/binary>>,State,Config) ->  header_top(Rest,[],new_http(get),[header_top|State],Config#state{method = get});
http(<<$P,$U,$T,$ ,Rest/binary>>,State,Config) ->  header_top(Rest,[],new_http(put),[header_top|State],Config#state{method = put});
http(<<$H,$E,$A,$D,$ ,Rest/binary>>,State,Config) ->  header_top(Rest,[],new_http(head),[header_top|State],Config#state{method = head});
http(<<$O,$P,$T,$I,$O,$N,$S,$ ,Rest/binary>>,State,Config) ->  header_top(Rest,[],new_http(options),[header_top|State],Config#state{method = options});
http(<<$H,$T,$T,$P,$/,Rest/binary>>,State,Config) ->  header_top(Rest,[],new_http_reponse(),[header_top|State],Config#state{method = http});
http(_,_,_) -> {wrong_data,<<"Not a HTTP Request">>}.

header_top(<<$ ,Rest/binary>>,Acc,DataHolder,State,Config) -> header_top(Rest,[], save_item(Acc,DataHolder),State,Config);
header_top(<<$\n,Rest/binary>>,Acc,DataHolder,[header_top|State],Config) ->  header(Rest,new_header(DataHolder,Acc),[http|State],Config);
header_top(<<$\r,Rest/binary>>,Acc,DataHolder,State,Config) -> header_top(Rest,Acc,DataHolder,State,Config);
header_top(<<H,Rest/binary>>,Acc,DataHolder,State,Config) -> header_top(Rest,[H|Acc],DataHolder,State,Config);
header_top(<<>>,Acc,DataHolder,[header_top|State],Config) -> {incomplete,{DataHolder,[{header_top,Acc}|State],Config}}.

header(<<$\r,Rest/binary>>,DataHolder,State,Config) -> header(Rest,DataHolder,State,Config);
header(<<$ ,Rest/binary>>,DataHolder,State,Config) -> header(Rest,DataHolder,State,Config);
header(<<$\n,_Rest/binary>>,_DataHolder,#state{content_length = undefined} =_,_Config) -> {wrong_data,<<"Content Length is missing">>};
header(<<$\n,Rest/binary>>,DataHolder,State,Config) -> body(Rest,new_body(DataHolder),[body|State],Config);
header(<<>>,DataHolder,State,Config) -> {incomplete,{DataHolder,State,Config}};
header(Rest,DataHolder,State,Config) -> header_key(Rest,[],DataHolder,[key|State],Config).

header_key(<<"Content-Type",Rest/binary>>,_,DataHolder,State,Config) -> header_key(Rest,"epyT-tnetnoC",DataHolder,State,Config#state{header_key = content_type});
header_key(<<"Content-Length",Rest/binary>>,_,DataHolder,State,Config) -> header_key(Rest,"htgneL-tnetnoC",DataHolder,State,Config#state{header_key = content_length});
header_key(<<$=,Rest/binary>>,Acc,DataHolder,[key|State],Config) -> header_val(Rest,[], save_item(Acc,DataHolder),[value|State],Config);
header_key(<<$:,Rest/binary>>,Acc,DataHolder,[key|State],Config) -> header_val(Rest,[], save_item(Acc,DataHolder),[value|State],Config);
header_key(<<$",Rest/binary>>,Acc,DataHolder,State,Config) -> header_key(Rest,Acc,DataHolder,State,Config);
header_key(<<$ ,Rest/binary>>,Acc,DataHolder,State,Config) -> header_key(Rest,Acc,DataHolder,State,Config);
header_key(<<H,Rest/binary>>,Acc,DataHolder,State,Config) -> header_key(Rest,[H|Acc],DataHolder,State,Config);
header_key(<<>>,Acc,DataHolder,[key|State],Config) -> {incomplete,{DataHolder,[{key,Acc}|State],Config}}.

header_val(<<$\r,Rest/binary>>,Acc,DataHolder,State,Config) -> header_val(Rest,Acc,DataHolder,State,Config);
header_val(<<$\n,Rest/binary>>,Acc,DataHolder,[value|State],Config = #state{header_key = content_type}) ->
  T = binary_util:strip(iolist_to_binary(lists:reverse(Acc))),
  header(Rest,save(T,DataHolder),State,Config#state{header_key = unknown,content_type = http_data:decide_content_type(T)});
header_val(<<$\n,Rest/binary>>,Acc,DataHolder,[value|State],Config = #state{header_key = content_length}) ->
  T = list_to_integer(string:strip(lists:reverse(Acc))),
  header(Rest,save(T,DataHolder),State,Config#state{header_key = unknown, content_length = T});
header_val(<<$\n,Rest/binary>>,Acc,DataHolder,[value|State],Config) -> header(Rest,save_item(string:strip(Acc),DataHolder),State,Config);
header_val(<<H,Rest/binary>>,Acc,DataHolder,State,Config) -> header_val(Rest,[H|Acc],DataHolder,State,Config);
header_val(<<>>,Acc,DataHolder,[value|State],Config) -> {incomplete,{DataHolder,[{value,Acc}|State],Config}}.

body(<<>>,{HttpTop,Headers,[]},[body,http],Config = #state{content_type = unknown,content_length = unknown}) ->  {complete,http_data(HttpTop,Headers,<<>>,Config)};
body(Source,{HttpTop,Headers,Body},[body,http]=State,Config = #state{content_type = unknown,content_length = Length}) ->
  NewBody = iolist_to_binary([Body,Source]),
  case size(NewBody) >= Length of
    true -> {complete,http_data(HttpTop,Headers,NewBody,Config)};
    false -> {incomplete,{{HttpTop,Headers,NewBody},State,Config}}
  end;
body(Source,{HttpTop,Headers,DataHolder},[body,http]=State,Config = #state{content_type = ContentType}) ->
  handle_parser_response(parserl:decode_stream(ContentType,Source,DataHolder),{HttpTop,Headers},State,Config);
body(<<>>,{HttpTop,Headers,Body},[body,http],Config = #state{content_length = unknown}) -> {complete,http_data(HttpTop,Headers,Body,Config)};
body(Source,{HttpTop,Headers,Body},[body,http]=State,Config = #state{content_length = unknown}) ->
  {incomplete,{{HttpTop,Headers,iolist_to_binary([Body,Source])},State,Config}};
body(Source,{HttpTop,Headers,Body},[body,http]=State,Config = #state{content_length = Length}) ->
  NewBody = iolist_to_binary([Body,Source]),
  case size(NewBody) >= Length of
    true -> {complete,http_data(HttpTop,Headers,NewBody,Config)};
    false -> {incomplete,{{HttpTop,Headers,NewBody},State,Config}}
  end.

handle_parser_response({complete,JsonData},{HttpTop,Headers},_State,Config) -> {complete,http_data(HttpTop,Headers,JsonData,Config)};
handle_parser_response({incomplete,NewJsonData},{HttpTop,Headers},State,Config) ->  {incomplete,{{HttpTop,Headers,NewJsonData},State,Config}};
handle_parser_response({wrong_data,Reason},_,_State,_Config) ->  {wrong_data,Reason}.

new_body([{header,Headers}|HeaderTop]) -> {HeaderTop,Headers,[]};
new_body(_) -> {wrong_data,<<" new body ">>}.

new_http_reponse() -> {http_response}.
new_http(Method) -> {header_top,Method}.
new_header({header_top,Method,Path},HttpVersion) -> [{header,[]},{Method,Path,iolist_to_binary(lists:reverse(HttpVersion))}];
new_header({http_response,Verion,SCode,STag},STagnew) -> NewTagBin = iolist_to_binary(lists:reverse(STagnew)),
  [{header,[]},{Verion,SCode,<<STag/binary,NewTagBin/binary>>}];
new_header({http_response,Verion,SCode},STagnew) ->  [{header,[]},{Verion,SCode,iolist_to_binary(lists:reverse(STagnew))}].

save_item(Item,DataHolder) -> save(iolist_to_binary(lists:reverse(Item)),DataHolder).

save(VersoinCode,{http_response}) -> {http_response,VersoinCode};
save(StatusCode,{http_response,VersionCode}) -> {http_response,VersionCode,StatusCode};
save(StatusTag,{http_response,VersionCode,StatusCode}) -> {http_response,VersionCode,StatusCode,StatusTag};
save(StatusTag2,{http_response,VersionCode,StatusCode,StatusTag}) ->  {http_response,VersionCode,StatusCode,<<StatusTag/binary,StatusTag2/binary>>};

save(Path,{header_top,Method}) -> {header_top,Method,Path};
save(Key,[{header,Headers}|Data]) -> [{header,Key,Headers}|Data];
save(Value,[{header,Key,Headers}|Data]) -> [{header,[{Key,Value}|Headers]}|Data];
save(_A1,_A2) -> io:format("An ~p ~n",[{_A1,_A2}]),{wrong_data,<<" save content problem">>}.


http_data([{http,Code,_Description}],Headers,Body,#state{method = http,content_type = CType } = _) ->
  http_data:set_content_type(CType,http_data:body(Body,http_data:headers(Headers,http_data:set_code(Code,http_data:new_response()))));
http_data([{post,Action,Version}],Headers,Body,#state{content_type = CType } = _) ->
  http_data:post(http_data:set_content_type(CType,http_data:body(Body,http_data:headers(Headers,http_data:version(Version,http_data:action(Action,http_data:new_request()))))));
http_data([{get,Action,Version}],Headers,Body,#state{content_type = CType } = _) ->
  http_data:get(http_data:set_content_type(CType,http_data:body(Body,http_data:headers(Headers,http_data:version(Version,http_data:action(Action,http_data:new_request()))))));
http_data([{put,Action,Version}],Headers,Body,#state{content_type = CType } = _) ->
  http_data:put(http_data:set_content_type(CType,http_data:body(Body,http_data:headers(Headers,http_data:version(Version,http_data:action(Action,http_data:new_request()))))));
http_data([{head,Action,Version}],Headers,Body,#state{content_type = CType } = _) ->
  http_data:head(http_data:set_content_type(CType,http_data:body(Body,http_data:headers(Headers,http_data:version(Version,http_data:action(Action,http_data:new_request()))))));
http_data([{options,Action,Version}],Headers,Body,#state{content_type = CType } = _) ->
  http_data:option(http_data:set_content_type(CType,http_data:body(Body,http_data:headers(Headers,http_data:version(Version,http_data:action(Action,http_data:new_request()))))));
http_data([{Version,Code,_Des}],Headers,Body,#state{content_type = CType } = _) ->
  http_data:version(Version,http_data:set_content_type(CType,http_data:body(Body,http_data:headers(Headers,http_data:set_code(Code,http_data:new_response()))))).



