-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-module(sync_data).
-export([start/0]).

-record(vcard, {us = {<<"">>, <<"">>} :: {binary(), binary()} | binary(),
                vcard = #xmlel{} :: xmlel()}).

start() ->
	{atomic, Keys} = mnesia:transaction(fun() -> mnesia:all_keys(vcard) end),
	{ok, P} = {ok, P} = python:start(),
	handle_key_data(Keys, P).

handle_key_data([{User, Server} | Users], P) ->
	{atomic, Vcard} = mnesia:transaction(fun() -> mnesia:read({vcard, {User, Server}} ) end),
    Data = Vcard#vcard.vcard,
    Name = xml:get_path_s(Data, [{elem, <<"NICKNAME">>}, cdata]),
    InterestList = xml:get_path_s(Data, [{elem, <<"fav_list">>}, cdata]),
    python:call(P, sync_data, handle_interest, [InterestList, User]),
    python:call(P, sync_data, handle_name, [Name, User]),
    handle_key_data(Users, P);

handle_key_data([], P) ->
	ok.



