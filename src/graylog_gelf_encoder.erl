-module(graylog_gelf_encoder).

-export([encode/6]).

-define(GELF_VERSION, <<"1.1">>).

encode(Severity, Msg, Timestamp, Host, Metadata, Compression) ->
    RawData = get_raw_data(Severity, Msg, Timestamp, Host, Metadata),
    EncodedData = safe_encode(RawData),
    compress(EncodedData, Compression).

get_raw_data(Level, Msg, Timestamp, Hostname, Metadata) ->
    [{<<"version">>, ?GELF_VERSION},
     {<<"level">>, severity2int(Level)},
     {<<"short_message">>, term2bin(Msg)},
     {<<"timestamp">>, unix_timestamp(Timestamp)},
     {<<"host">>, term2bin(Hostname)}
     | format_metadata(Metadata)].

format_metadata(Metadata) ->
    format_metadata(Metadata, []).

format_metadata([{K, V} | T], Acc) ->
    format_metadata(T, [{<<"_", (term2bin(K))/binary>>, term2bin(V)} | Acc]);
format_metadata([], Acc) ->
    Acc.

safe_encode(Msg) ->
    case catch jiffy:encode({Msg}, [force_utf8]) of
      JsonPayloadBin when is_binary(JsonPayloadBin) ->
          JsonPayloadBin;
      JsonPayloadList when is_list(JsonPayloadList) ->
          iolist_to_binary(JsonPayloadList);
      {error, _} ->
          {value, {_, InnerMsg}, Msg1} = lists:keytake(<<"short_message">>, 1, Msg),
          InnerMsg2 = iolist_to_binary(io_lib:format("hex msg. json encode failed: ~p",
                                                     [graylog_hex:bin2hex(term_to_binary(InnerMsg))])),
          safe_encode([{<<"short_message">>, InnerMsg2} | Msg1])
    end.

compress(Data, disabled) ->
    Data;
compress(Data, gzip) ->
    zlib:gzip(Data);
compress(Data, zlib) ->
    zlib:compress(Data).

unix_timestamp({Mega, Sec, Micro}) ->
    Mega * 1000000 + Sec + Micro / 1000000.

term2bin(L) when is_binary(L) ->
    L;
term2bin(L) when is_list(L) ->
    iolist_to_binary(L);
term2bin(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
term2bin(P) when is_integer(P) ->
    integer_to_binary(P);
term2bin(P) when is_float(P) ->
    float_to_binary(P, [{decimals, 4}, compact]);
term2bin(P) when is_pid(P) ->
    list_to_binary(pid_to_list(P));
term2bin(Other) ->
    list_to_binary(io_lib:format("~p", [Other])).

severity2int(debug) ->
    7;
severity2int(info) ->
    6;
severity2int(notice) ->
    5;
severity2int(warning) ->
    4;
severity2int(error) ->
    3;
severity2int(critical) ->
    2;
severity2int(alert) ->
    1;
severity2int(emergency) ->
    0;
severity2int(_) ->
    7.

