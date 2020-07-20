-module(graylog_udp_sender).

-export([open/2, open/3, send/2]).

-record(state, {address, socket, port, chunk_size}).

-define(CHUNK_GELF_ID, <<30, 15>>).
-define(CHUNK_SIZE, 1472).

open(Host, Port) ->
    open(Host, Port, []).

open(Host, Port, Options) ->
    ChunkSize = proplists:get_value(chunk_size, Options, ?CHUNK_SIZE),
    InetFamily = proplists:get_value(inet_family, Options, inet),
    validate_conf(host, Host),
    validate_conf(port, Port),
    validate_conf(chunk_size, ChunkSize),
    validate_conf(inet_family, InetFamily),
    {ok, Address} = inet:getaddr(Host, InetFamily),
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}, {sndbuf, 60000}]),
    {ok, #state{socket = Socket, address = Address, port = Port, chunk_size = ChunkSize}}.

validate_conf(host, undefined) ->
    throw({error, invalid_host});
validate_conf(port, Port) ->
    case Port of
      P when P >= 1 andalso P =< 65536 ->
          true;
      _ ->
          throw({error, invalid_port})
    end;
validate_conf(inet_family, F) when F =/= inet6 andalso F =/= inet ->
    throw({error, invalid_inet});
validate_conf(chunk_size, Value) when is_integer(Value) == false ->
    throw({error, invalid_chunk_size});
validate_conf(_, _) ->
    true.

send(State, Msg) ->
    send(State, Msg, byte_size(Msg)).

send(State, Msg, MsgLength) when MsgLength =< State#state.chunk_size ->
    ok = gen_udp:send(State#state.socket, State#state.address, State#state.port, Msg);
send(#state{chunk_size = ChunkSize} = State, Msg, MsgLength) ->
    ChunksNumber = get_chunks_number(MsgLength, ChunkSize),
    chunk_send(crypto:strong_rand_bytes(8), 0, ChunksNumber, ChunkSize, MsgLength, Msg, State).

chunk_send(_ChunkId,
           _SequenceNumber,
           _NumberOfChunks,
           _ChunkSize,
           _BodyLength,
           <<>>,
           _State) ->
    ok;
chunk_send(ChunkId, SequenceNumber, NumberOfChunks, ChunkSize, BodyLength, Body, State) ->
    RealChunkSize = erlang:min(ChunkSize, BodyLength),
    <<BodyPart:RealChunkSize/binary, Rest/binary>> = Body,
    ChunkData = <<?CHUNK_GELF_ID/binary,
                  ChunkId/binary,
                  SequenceNumber:8/integer,
                  NumberOfChunks:8/integer,
                  BodyPart/binary>>,
    ok = gen_udp:send(State#state.socket, State#state.address, State#state.port, ChunkData),
    chunk_send(ChunkId,
               SequenceNumber + 1,
               NumberOfChunks,
               ChunkSize,
               BodyLength - RealChunkSize,
               Rest,
               State).

get_chunks_number(PayloadSize, ChunkSize) when ChunkSize < PayloadSize ->
    ChunksPerPayload = PayloadSize / ChunkSize,
    T = trunc(ChunksPerPayload),
    case ChunksPerPayload - T == 0 of
      true ->
          ChunksPerPayload;
      _ ->
          T + 1
    end.

