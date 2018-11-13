-module (notes_server).

-behaviour (gen_server).

-export ([start_link/0]).
-export([init/1,handle_call/3,handle_cast/2,
	handle_info/2,terminate/2,code_change/3]).

-include ("../include/notes.hrl").

-define (SERVER , ?MODULE).

-record (state, {}).

start_link() ->
	gen_server:start_link({local,?SERVER},?MODULE,[],[]).

init([]) ->
	process_flag(trap_exit,true),
	error_logger:info_report("initializing database.....") ,
	db:start(),
	error_logger:info_report("database started....."),
	{ok,#state{}}.

handle_call({add_note,Title,Content},_From,State) ->
	{ok,Note} = db_access:add_notes(Title,Content),
	{reply,Note,State};

handle_call({get_note,Id},_From,State) ->
	{ok,Note} = db_access:get_note(Id),
	{reply,Note,State};

handle_call(get_all_note,_From,State) ->
	{ok,Notes} = db_access:get_notes(),
	{reply,Notes,State};

handle_call({update_note,Id,Title,Content},_From,State) ->
	{ok,N} = db_access:get_note(Id),
	{ok,Note} = db_access:update_note(N#notes.id,Title,Content),
	{reply,Note,State};

handle_call({delete_note,Id},_From,State) ->
	{ok,Note} = db_access:get_note(Id),
	{atomic,Result} = mnesia:transaction(fun() ->
		mnesia:delete_object(Note)
	end),
	{reply,Result,State};

handle_call({search_note,Title},_From,State) ->
	{ok,Notes} = db_access:search_notes(Title),
	{reply,Notes,State}.

handle_cast(stop,State) -> {stop,normal,State};

handle_cast(_Msg,State) -> {noreply,State}.

handle_info(_Info,State) -> {noreply,State}.

terminate(_Reason,_State) ->
	io:format("~p stopping~n",[?MODULE]),
	ok.

code_change(_OldVsn,State,_Extra) -> {ok,State}.