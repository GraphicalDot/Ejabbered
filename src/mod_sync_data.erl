-module(mod_sync_data).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-export([start/0]).

-record(vcard, {us = {<<"">>, <<"">>} :: {binary(), binary()} | binary(),
                vcard = #xmlel{} :: xmlel()}).

start() ->
	{atomic, Keys} = mnesia:transaction(fun() -> mnesia:all_keys(vcard) end),
    handle_key_data(Keys).

handle_key_data([{User, Server} | Users]) ->
    {atomic, [Vcard| _]} = mnesia:transaction(fun() -> mnesia:read({vcard, {User, Server}} ) end),
    Data = Vcard#vcard.vcard,
    Name =   xml:get_path_s(Data, [{elem, <<"NICKNAME">>}, cdata]),
    handle_name(Name, User, Server),
    try jiffy:decode(xml:get_path_s(Data, [{elem, <<"fav_list">>}, cdata])) of
        Interests ->
            handle_interest(Interests, User, Server)
    catch
        _ -> ok
    end,
	handle_key_data(Users);

handle_key_data([]) ->
    ok.

handle_name(Name, User, Server) ->
    ejabberd_odbc:sql_query(
           Server,
           [<<"update users set name = " >>, 
                <<"'">>, Name, <<"'">>,
                <<" where username  = ">>,
                <<"'">>, User, <<"' and name is null;">>]).


handle_interest([{Interest}|Interests], User, Server) ->
    add_interest(Interest, User, Server),
    handle_interest(Interests, User, Server);

handle_interest([], User, Server) ->
    ok.



add_interest(Interest, User, Server) ->
    InterestId = proplists:get_value(<<"obj_id">>, Interest),
    case ejabberd_odbc:sql_query(
           Server,
           [<<"insert into users_interest (interest_id, username) values " >>, 
                <<"(">>,
                    <<"'">>, InterestId, <<"', ">>,
                    <<"'">>, User, <<"'">>,
                <<");">>]) of
    _ ->
        ok
    end.