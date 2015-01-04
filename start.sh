#!/bin/sh
erl +K true +P 10240000 -sname httpserver -pa ebin -pa deps/*/ebin  -s sei_http_app 
    -eval "io:format(\"Server start with port 8000 Success!~n\")." 
	> server.log

