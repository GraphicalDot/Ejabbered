%% name of module must match file name
-module(mod_set_available_status).

-define(SUPERVISOR, ejabberd_sup).

%% Every ejabberd module implements the gen_mod behavior
%% The gen_mod behavior requires two functions: start/2 and stop/1
%% gen_server has also been implemented to allow asynchronous operations

-behaviour(gen_mod).
-behaviour(gen_server).

%% gen_mod callbacks
-export([start/2, stop/1, start_link/2]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
     handle_cast/2, handle_info/2, code_change/3]).

%% Hook callbacks
-export([on_user_unregister_connection/3,
        on_user_register_connection/3,
        on_user_unavailable/2,
        on_user_available/2,
        on_user_send_message/4
]).

%% included for writing to ejabberd log file
-include("ejabberd.hrl").
-include("logger.hrl").

%% ejabberd functions for JID manipulation called jlib.
-include("jlib.hrl").
%%add and remove hook module on startup and close

-record(state, {
    host = <<"">>
    }).

%%==================================================
%% gen mod callbacks
%%==================================================

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    PingSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
        transient, 2000, worker, [?MODULE]},
    supervisor:start_child(?SUPERVISOR, PingSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:delete_child(?SUPERVISOR, Proc).



%%==================================================
%% gen server callbacks
%%==================================================

init([Host, Opts]) ->
    ejabberd_hooks:add(status_offline_hook, Host, ?MODULE, on_user_unavailable, 100),
    ejabberd_hooks:add(status_online_hook, Host, ?MODULE, on_user_available, 100),
    ejabberd_hooks:add(user_unavailable_hook, Host, ?MODULE, on_user_unavailable, 100),
    ejabberd_hooks:add(user_started_sending_message, Host, ?MODULE, on_user_send_message, 100),
    ejabberd_hooks:add(sm_remove_connection_hook, Host, ?MODULE, on_user_unregister_connection, 100),
    ejabberd_hooks:add(sm_register_connection_hook, Host, ?MODULE, on_user_register_connection, 100),
    {ok, #state{host = Host}}.

terminate(_Reason, #state{host = Host}) ->
    ejabberd_hooks:add(status_offline_hook, Host, ?MODULE, on_user_unavailable, 100),
    ejabberd_hooks:add(status_online_hook, Host, ?MODULE, on_user_available, 100),
    ejabberd_hooks:delete(user_unavailable_hook, Host, ?MODULE, on_user_unavailable, 100),
    ejabberd_hooks:delete(user_started_sending_message, Host, ?MODULE, on_user_send_message, 100),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host, ?MODULE, on_user_unregister_connection, 100),
    ejabberd_hooks:delete(sm_register_connection_hook, Host, ?MODULE, on_user_register_connection, 100).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({update_availability, User, Server, IsAvailabileStatus}, State) ->
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
            ?ERROR_MSG(" Encountered error in mod_set_available_status ~p ~n ", [Error]) 
    end,    
    {noreply, State};

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%==================================================
%% API
%%==================================================

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE,
              [Host, Opts], []).


on_user_send_message(C2SState, User, Server) -> 
    set_available(User, Server).

on_user_send_message(Packet, _C2SState, _, _) ->
    Packet.

on_user_presence_update(Packet, User, Server) -> 
    Packet.

on_user_unavailable(User, Server) ->
    set_unavailable(User, Server).

on_user_available(User, Server) ->
    set_available(User, Server).

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

set_available(User, Host) -> 
    IsAvailabileStatus = <<"True">>,
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {update_availability, User, Host, IsAvailabileStatus}).

set_unavailable(User, Host) ->
    IsAvailabileStatus = <<"False">>,
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {update_availability, User, Host, IsAvailabileStatus}).

