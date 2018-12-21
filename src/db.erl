-module (db).

-export ([install/1,
	create_schema/1,
	ensure_loaded/0,
	start/0,start/1]).

-include ("../include/notes.hrl").

%%=================== mnesia configurations =================================
create_schema(Node) ->
	mnesia:create_schema(Node).

install(Node) ->
	mnesia:create_table(notes,[
						{attributes,record_info(fields,notes)},
						{index,[#notes.title]},
						{disc_copies,Node},
						{type,ordered_set}]),
	ok.

ensure_loaded() ->
	ok = mnesia:wait_for_tables([notes],5000).

start() ->
	start([node()|nodes()]).

start(Node) ->
	case is_fresh_startup() of
		true ->
			case mnesia:system_info(is_running) of
				yes ->
					error_logger:info_report("stopping mnesia"),
					rpc:multicall(Node,application,stop,[mnesia]);
				_   -> pass
			end, 

			create_schema(Node),
			error_logger:info_report("mnesia schema created"),
			error_logger:info_report("starting mnesia"),
			rpc:multicall(Node,application,start,[mnesia]),
			install(Node),
			error_logger:info_report("mnesia tables created");
		{exists, _ } ->
			ok = ensure_loaded()
	end.

%%==========================================================================

%%=========================== private function =============================
is_fresh_startup() ->
	Node = [node()|nodes()],
	case mnesia:system_info(tables) of
		[schema] -> true;
		Tables ->
			case mnesia:table_info(schema,cookie) of
				{ _ , Node} -> {exists,Tables};
					_       -> true
			end 
	end.
