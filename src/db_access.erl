-module (db_access).
-export ([add_notes/2,get_note/1,get_notes/0,search_notes/1,
			update_note/3]).
-include ("../include/notes.hrl").
-include_lib ("stdlib/include/qlc.hrl").

add_notes(Title,Content) ->

	{{Y,M,D},{H,Mi,S}} = erlang:localtime(),
	Time = lists:concat([H,":",Mi,":",S,"/",D,"-",M,"-",Y]),

	{atomic,Note} = mnesia:transaction(fun() ->
		NextId = erlang:system_time(nano_seconds),

		New = #notes{
			id = NextId,
			title = Title,
			content = Content,
			created = Time,
			updated = Time 
		},
		mnesia:write(New),
		New
	end),
	{ok,Note}.

get_note(Id) ->
	{atomic,Note} = mnesia:transaction(fun() ->
		case mnesia:read(notes,Id,read) of
			[Item] -> Item;
			[] -> []
		end
	end),
	{ok,Note}.

get_notes() ->
	{atomic,Notes} = mnesia:transaction(fun() ->
		Q = qlc:q([N || N <- mnesia:table(notes)]),
		qlc:e(Q)
	end),
	{ok,Notes}.

search_notes(Title) ->
	{atomic,Notes} = mnesia:transaction(fun() ->
		Q = qlc:q([N || N <- mnesia:table(notes), N#notes.title == Title]),
		qlc:e(Q)
	end),
	{ok,Notes}.
 
update_note(Id,Title,Content) ->
	{{Y,M,D},{H,Mi,S}} = erlang:localtime(),
	Time = lists:concat([H,":",Mi,":",S,"/",D,"-",M,"-",Y]),
	{atomic, Note} = mnesia:transaction(fun() ->
		Update = #notes{
			id = Id,
			title = Title,
			content = Content,
			created = Time,
			updated = Time
		},
		mnesia:write(Update),
		Update
	end),
	{ok,Note}.

