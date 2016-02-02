%% name of module must match file name
-module(mod_location).

%% Every ejabberd module implements the gen_mod behavior
%% The gen_mod behavior requires two functions: start/2 and stop/1
-behaviour(gen_mod).

%% public methods for this module
-export([start/2, stop/1]).
-export([on_user_presence_update/3, 
        code_change/3, 
        on_user_unregister_connection/3,
        on_user_register_connection/3
]).

%% included for writing to ejabberd log file
-include("ejabberd.hrl").
-include("logger.hrl").

%% ejabberd functions for JID manipulation called jlib.
-include("jlib.hrl").
%%add and remove hook module on startup and close

%%==================================================
%% gen server callbacks
%%==================================================

start(Host, Opts) ->
    ?PRINT("starting mod_location",[]),
    ejabberd_hooks:add(c2s_update_presence, Host, ?MODULE, on_user_presence_update, 100),
    ejabberd_hooks:add(sm_remove_connection_hook, Host, ?MODULE, on_user_unregister_connection, 100),
    ejabberd_hooks:add(sm_register_connection_hook, Host, ?MODULE, on_user_register_connection, 100),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(c2s_update_presence, Host, ?MODULE, on_user_presence_update, 100),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host, ?MODULE, on_user_unregister_connection, 100),
    ejabberd_hooks:delete(sm_register_connection_hook, Host, ?MODULE, on_user_register_connection, 100),
    ok.

%%==================================================
%% callbacks
%%==================================================

code_change(_OldVsn, State, _Extra) -> {ok, State}.


on_user_presence_update(#xmlel{name = <<"presence">>} = Packet, User, Server) ->
    case xml:get_subtag_cdata(Packet, <<"status">>) of
        <<"Offline">> ->
            set_unavailable(User, Server);
        <<"Online">> ->
            set_available(User, Server);
        _ ->  ok
    end,
    Packet;

on_user_presence_update(Packet, User, Server) -> 
    Packet.

on_user_unregister_connection(_, #jid{luser = LUser, lserver = LServer}, _) ->
    set_unavailable(LUser, LServer);

on_user_unregister_connection(_, _, _) ->
    ok.

on_user_register_connection(_, #jid{luser = LUser, lserver = LServer}, _) ->
    set_available(LUser, LServer);

on_user_register_connection(_, _, _) ->
    ok.

%%==================================================
%% internal functions
%%==================================================

set_available(User, Server) -> 
    IsAvailabileStatus = <<"True">>,
    update_availability(User, Server, IsAvailabileStatus).

set_unavailable(User, Server) ->
    IsAvailabileStatus = <<"False">>,
    update_availability(User, Server, IsAvailabileStatus).


update_availability(User, Server, IsAvailabileStatus) ->
    case ejabberd_odbc:sql_query(
           Server,
           [<<"update users " >>,
                <<" SET is_available  =  ">>,
                IsAvailabileStatus,  
                <<" where username  = ">>,
                <<"'">>, User, <<"';">>]) of
          
        {updated, 1} -> 
            ok;
        Error -> 
            ?ERROR_MSG(" Encountered error in mod_location ~p ~n ", [Error]) 
    end.        


