%% name of module must match file name
-module(mod_location).

%% Every ejabberd module implements the gen_mod behavior
%% The gen_mod behavior requires two functions: start/2 and stop/1
-behaviour(gen_mod).

%% public methods for this module
-export([start/2, stop/1]).
-export([on_user_presence_update/3, code_change/3]).

%% included for writing to ejabberd log file
-include("ejabberd.hrl").
-include("logger.hrl").

%% ejabberd functions for JID manipulation called jlib.
-include("jlib.hrl").
%%add and remove hook module on startup and close

start(Host, Opts) ->
    ?PRINT("starting mod_location",[]),
    ejabberd_hooks:add(c2s_update_presence, Host, ?MODULE, on_user_presence_update, 100),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(c2s_update_presence, Host, ?MODULE, on_user_presence_update, 100),
    ok.

on_user_presence_update(#xmlel{name = <<"presence">>} = Packet, User, Server) ->
    Status =  case xml:get_subtag_cdata(Packet, <<"status">>) of
        <<"Offline">> ->
            {ok, <<"False">>};
        <<"Online">> ->
            {ok, <<"True">>};
        _ ->  ok
    end,

    case Status of
        {ok, IsAvailabileStatus} -> 
            update_availability(User, Server, IsAvailabileStatus);
        _ -> 
            ok
    end,
    Packet;

on_user_presence_update(Packet, User, Server) -> Packet.

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

code_change(_OldVsn, State, _Extra) -> {ok, State}.
