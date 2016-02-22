-module(mod_apple_push).
-author("satishck1992@gmail.com").
-behaviour(gen_mod).

-define(GEN_SERVER, p1_server).
-behaviour(?GEN_SERVER).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include_lib("apns/include/apns.hrl").

-export([start/2, stop/1, start_link/2, mod_opt_type/1]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).


-export([handle_push_notification/3,
        handle_error/2, 
        handle_app_deletion/1, 
        on_user_going_offline/4,
        on_user_coming_online/1]).


-define(DEFAULT_APPLE_HOST, "gateway.sandbox.push.apple.com").
-define(DEFAULT_APPLE_PORT, 2195).
-define(DEFAULT_CERT, undefined).
-define(DEFAULT_KEY, undefined).
-define(DEFAULT_KEY_FILE, undefined).
-define(DEFAULT_CERT_PASSWORD, "pushchat").
-define(DEFAULT_TIMEOUT, 30000).
-define(DEFAULT_FEEDBACK_HOST, "feedback.sandbox.push.apple.com").
-define(DEFAULT_FEEDBACK_PORT, 2196).
-define(DEFAULT_FEEDBACK_TIMEOUT, 30*60*1000).
-define(DEFAULT_EXPIRES_CONN, 300).
-define(DEFAULT_EXTRA_SSL_OPTS, []).

-define(DICT, dict).

-record(state,{host = <<"">> : binary(),
        apns_connection_name :: atom(), 
        ios_offline_users = (?DICT):new() 
    }).


%%=======================================nxdomain=================
%% gen_mod callbacks
%%========================================================


start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
     transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    catch ?GEN_SERVER:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.


%%====================================================================
%% hook callbacks
%%====================================================================

on_user_going_offline(User, Server, _Resource, _Status) ->
    Proc = gen_mod:get_module_proc(Server, ?MODULE),
    case get_udid_for_user(User, Server) of
        {ok, null} ->
            ok;
        {ok, Udid} ->
            gen_server:cast(Proc, {add_user_to_notification_list, User, Udid});
        _ -> 
            ok
    end,
    ok.


on_user_coming_online(#jid{luser = User, lserver = Server} = JID) ->
    Proc = gen_mod:get_module_proc(Server, ?MODULE),
    gen_server:cast(Proc, {remove_user_from_notification_list, User}),
    ok.


%%====================================================================
%% gen_server callbacks
%%====================================================================

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ?GEN_SERVER:start_link({local, Proc}, ?MODULE,
                           [Host, Opts], []).

init([Host, Opts]) ->
  ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, handle_push_notification, 50),
  ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, on_user_going_offline, 50),
  ejabberd_hooks:add(user_available_hook, Host, ?MODULE, on_user_coming_online, 50),

  ApnsConnectionName = apple_connection, 
    case apns:connect(
        ApnsConnectionName,
        get_apns_connection_info(Opts)
    )
        of
        {error, Reason} -> 
            ?ERROR_MSG(" mod_apple_push failed to start due to ~p ~n ", [Reason]),
            {stop, Reason};
        _ ->
            State = #state{
                host = Host,
                apns_connection_name = ApnsConnectionName
            },
           {ok, State}
      end.

terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, handle_push_notification, 50),
    ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, on_user_going_offline, 50),
    ejabberd_hooks:delete(user_available_hook, Host, ?MODULE, on_user_coming_online, 50),
    ok.


%%====================================================================
%% gen_server callbacks
%%====================================================================

handle_cast({add_user_to_notification_list, User, IosDeviceId}, State) ->
    #state{ios_offline_users = IosOfflineUsers} = State,
    AlteredDict = (?DICT):erase(User, IosOfflineUsers),
    NewDict = (?DICT):store(User, IosDeviceId, AlteredDict),
    NewState = State#state{ios_offline_users = NewDict},    
    {noreply, NewState};

handle_cast({remove_user_from_notification_list, User}, State) ->
    #state{ios_offline_users = IosOfflineUsers} = State,
    AlteredDict = (?DICT):erase(User, IosOfflineUsers),
    NewState = State#state{ios_offline_users = AlteredDict},    
    {noreply, NewState};

handle_cast(_Info, State) -> 
    {noreply, State}.

handle_call({notify, Message, #jid{luser = User}},_From, State) -> 
    #state{ios_offline_users = IosOfflineUsers} = State,
    case (?DICT):find(User, IosOfflineUsers) of
        error ->
            ok;            
        {ok, Value} ->
            apns:send_message(State#state.apns_connection_name, #apns_msg{device_token = Value, alert = Message})
    end,
    {noreply, State};


handle_call(stop, _From, State) -> 
  {stop, normal, ok, State}.

handle_info(_Info, State) ->
    ?ERROR_MSG("got unexpected info: ~p", [_Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%====================================================================
%% apns callback functions
%%====================================================================
handle_error(MsgId, Status) ->
    ?ERROR_MSG("error: ~p - ~p~n", [MsgId, Status]),
  ok.

handle_app_deletion(_) ->
  ok.


%%====================================================================
%% gen_server internal functions
%%====================================================================

get_udid_for_user(User, Server) ->
    case ejabberd_odbc:sql_query(
       Server,
       [<<"select apple_udid from users " >>, 
            <<" where username  = ">>,
            <<"'">>, User, <<"';">>]) of

    {selected, _, [[Udid]]} -> 
        {ok, Udid};
    _ ->
        false
    end.

handle_push_notification(From, To, Packet) ->
    #jid{lserver = Server} = From,
    case xml:get_subtag_cdata(Packet, <<"body">>) of 
        <<>> ->
            ok;
        Body ->
            Notification = <<" You received a message ">>,
            notify(To, Notification, Server)
    end,
    Packet.

notify(To, Message, Host) ->
  Proc = gen_mod:get_module_proc(Host, ?MODULE),
  gen_server:call(Proc, {notify, Message, To}).

get_apns_connection_info(Opts) -> 
      #apns_connection{
            apple_host        = binary_to_list(gen_mod:get_opt(apple_host, Opts, fun(A) -> A end, false)),
            apple_port        = gen_mod:get_opt(apple_port, Opts, fun(A) -> A end, false),
            cert              = ?DEFAULT_CERT,
            cert_file         = gen_mod:get_opt(cert_file, Opts, fun(A) -> A end, false),
            key               = ?DEFAULT_KEY,
            key_file          = ?DEFAULT_KEY_FILE,
            cert_password     = binary_to_list(gen_mod:get_opt(cert_password, Opts, fun(A) -> A end, false)),
            timeout           = ?DEFAULT_TIMEOUT,
            feedback_host     = ?DEFAULT_FEEDBACK_HOST,
            feedback_port     = ?DEFAULT_FEEDBACK_PORT,
            feedback_timeout  = ?DEFAULT_FEEDBACK_TIMEOUT,
            expires_conn      = ?DEFAULT_EXPIRES_CONN,
            extra_ssl_opts    = ?DEFAULT_EXTRA_SSL_OPTS,
            error_fun = fun ?MODULE:handle_error/2,
            feedback_fun = fun ?MODULE:handle_app_deletion/1
        }.




mod_opt_type(apple_host) -> fun binary_to_list/1;
mod_opt_type(apple_port) ->
    fun (A) when is_integer(A) andalso A >= 0 -> A end;
mod_opt_type(cert_file) -> fun binary_to_list/1;
mod_opt_type(cert_password) -> fun binary_to_list/1;

mod_opt_type(_) -> [apple_host, apple_port, cert_file, cert_password].
