-module(graylog_lager_gelf_formatter).

-export([format/2, format/3]).

-define(GELF_VERSION, <<"1.1">>).

format(Message, Config, _Colors) ->
    format(Message, Config).

format(Message, Config) ->
    Compression = proplists:get_value(compression, Config, disabled),
    Severity = lager_msg:severity(Message),
    Msg = lager_msg:message(Message),
    Timestamp = lager_msg:timestamp(Message),
    Metadata = proplists:get_value(extra_fields, Config, []),
    Host = proplists:get_value(hostname, Config),
    graylog_gelf_encoder:encode(Severity, Msg, Timestamp, Host, Metadata, Compression).

