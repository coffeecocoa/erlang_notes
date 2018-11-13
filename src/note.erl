-module (note).
-export ([add/2,get/1,get_all/0,update/3,delete/1,search/1,
			stop/0]).
-include ("../include/notes.hrl").

add(Title,Content) ->
	gen_server:call(notes_server,{add_note,Title,Content}).

get(Id) ->
	gen_server:call(notes_server,{get_note,Id}).

get_all() ->
	gen_server:call(notes_server,get_all_note).

update(Id,Title,Content) ->
	gen_server:call(notes_server,{update_note,Id,Title,Content}).

delete(Id) ->
	gen_server:call(notes_server,{delete_note,Id}).

%% Searching with complete title for now
search(Title) -> 
	gen_server:call(notes_server,{search_note,Title}).

stop() -> gen_server:cast(notes_server,stop).
