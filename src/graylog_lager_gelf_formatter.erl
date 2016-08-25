-module(graylog_lager_gelf_formatter).
-author("silviu.caragea").

-export([format/2, format/3]).

-define(GELF_VERSION, <<"1.1">>).

format(Message, Config, _Colors) ->
    format(Message, Config).

format(Message, Config) ->
    JsonPayload = jsonx:encode(get_raw_data(Message)),
    do_compression(JsonPayload, graylog_lager_utils:lookup(compression, Config)).

get_raw_data(Message) ->
    [
        {version, ?GELF_VERSION},
        {level, graylog_lager_utils:severity2int(lager_msg:severity(Message))},
        {short_message, graylog_lager_utils:term2bin(lager_msg:message(Message))},
        {timestamp, graylog_lager_utils:unix_timestamp(lager_msg:timestamp(Message))},
        {host, graylog_lager_utils:hostname()} |
        get_metadata(Message)
    ].

get_metadata(Msg) ->
    get_metadata(lager_msg:metadata(Msg), []).

get_metadata([], Acc) ->
    Acc;
get_metadata([{K,V}|T], Acc) ->
    NewK = <<"_", (graylog_lager_utils:term2bin(K))/binary>>,
    get_metadata(T, [{NewK, graylog_lager_utils:term2json(V)} | Acc]).

do_compression(Data, undefined) ->
    Data;
do_compression(Data, gzip) ->
    zlib:gzip(Data);
do_compression(Data, zlib) ->
    zlib:compress(Data).