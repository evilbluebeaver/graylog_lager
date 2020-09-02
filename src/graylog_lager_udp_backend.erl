-module(graylog_lager_udp_backend).

-include_lib("lager/include/lager.hrl").

-behaviour(gen_event).

% http://docs.graylog.org/en/3.1/pages/gelf.html

-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {name, sender, level, formatter, format_config, shaper :: lager_shaper()}).

-define(CHUNK_SIZE, 1420).
-define(DEFAULT_GELF_FORMATTER, graylog_lager_gelf_formatter).

init(Config) ->
    Level = proplists:get_value(level, Config, debug),
    Formatter = proplists:get_value(formatter, Config, ?DEFAULT_GELF_FORMATTER),
    FormatConfig0 = proplists:get_value(format_config, Config, []),
    InetFamily = proplists:get_value(inet_family, Config, inet),
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config),
    Name = proplists:get_value(name, Config, {Host, Port}),
    ChunkSize = proplists:get_value(chunk_size, Config, ?CHUNK_SIZE),

    Flush = proplists:get_value(flush_queue, Config),
    HighWaterMark = proplists:get_value(high_water_mark, Config),
    FlushThr = proplists:get_value(flush_threshold, Config, 0),

    Shaper = lager_util:maybe_flush(Flush,
                                    #lager_shaper{hwm = HighWaterMark,
                                                  flush_threshold = FlushThr,
                                                  id = ?MODULE}),

    validate_conf(level, Level),

    FormatConfig = case proplists:get_value(hostname, FormatConfig0) of
                     undefined ->
                        {ok, Hostname} = inet:gethostname(),
                         [{hostname, Hostname} | FormatConfig0];
                     _ ->
                         FormatConfig0
                   end,
    {ok, Sender} = graylog_udp_sender:open(Host,
                                           Port,
                                           [{inet_family, InetFamily}, {chunk_size, ChunkSize}]),
    {ok,
     #state{level = lager_util:level_to_num(Level),
            name = {?MODULE, Name},
            sender = Sender,
            formatter = Formatter,
            format_config = FormatConfig,
            shaper = Shaper}}.

handle_call(get_loglevel, #state{level = Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    case lists:member(Level, ?LEVELS) of
      true ->
          {ok, ok, State#state{level = lager_util:level_to_num(Level)}};
      _ ->
          {ok, {error, bad_log_level}, State}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, MessageInner},
             #state{level = L,
                    shaper = Shaper,
                    sender = Sender,
                    name = Name,
                    formatter = Formatter,
                    format_config = FormatConfig} =
                 State) ->
    case lager_util:is_loggable(MessageInner, L, Name) of
      true ->
          case lager_util:check_hwm(Shaper) of
            {true, Drop, #lager_shaper{hwm = Hwm} = NewShaper} ->
                case Drop > 0 of
                  true ->
                      Report =
                          io_lib:format("graylog_lager_udp_backend dropped ~p messages in the last second "
                                        "that exceeded the limit of ~p messages/sec",
                                        [Drop, Hwm]),
                      write(lager_msg:new(Report, warning, [], []), Formatter, FormatConfig, State);
                  _ ->
                      ok
                end,
                write(Sender, MessageInner, Formatter, FormatConfig),
                {ok, State#state{shaper = NewShaper}};
            {false, _, #lager_shaper{dropped = D} = NewShaper} ->
                {ok, State#state{shaper = NewShaper#lager_shaper{dropped = D + 1}}}
          end;
      _ ->
          {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%internal

write(Sender, MessageInner, Formatter, FormatConfig) ->
    Msg = Formatter:format(MessageInner, FormatConfig),
    graylog_udp_sender:send(Sender, Msg).

validate_conf(level, L) ->
    case lists:member(L, ?LEVELS) of
      true ->
          true;
      _ ->
          throw({error, invalid_level})
    end;
validate_conf(_, _) ->
    true.

