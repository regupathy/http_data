%%%-------------------------------------------------------------------
%%% @author regupathy.b
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2016 1:01 PM
%%%-------------------------------------------------------------------
-author("regupathy.b").

-record(http_data,{for,request,action= <<"/">>,code,version= <<"HTTP/1.1">>,headers = [],body = <<>>,content_type,content_length=0}).
-record(http_response_code,{ok,accepted,websocket_accepted,bad_request,forbidden,not_found,
        internal_server_error,service_unavailable,  permission_denied}).
-record(content_type,{raw,json,xml,text,html}).
-record(method,{post,get,put,option,head}).














