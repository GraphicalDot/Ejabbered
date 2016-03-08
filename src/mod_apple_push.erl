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
        on_user_coming_online/1,
        start_connection/1]).


-define(DEFAULT_APPLE_HOST, "gateway.sandbox.push.apple.com").
-define(DEFAULT_APPLE_PORT, 2195).
-define(DEFAULT_CERT, undefined).
-define(DEFAULT_KEY, undefined).
-define(DEFAULT_KEY_FILE, undefined).
-define(DEFAULT_CERT_PASSWORD, "pushchatkey").
-define(DEFAULT_TIMEOUT, 30000).
-define(DEFAULT_FEEDBACK_HOST, "feedback.sandbox.push.apple.com").
-define(DEFAULT_FEEDBACK_PORT, 2196).
-define(DEFAULT_FEEDBACK_TIMEOUT, 30*60*1000).
-define(DEFAULT_EXPIRES_CONN, 300).
-define(DEFAULT_EXTRA_SSL_OPTS, []).

-define(DICT, dict).

-record(state,{host = <<"">> : binary(),
        apns_connection_name :: atom(), 
        ios_offline_users = (?DICT):new(),
        opts,
        notification_queue = queue:new(), 
        is_connected = false,
        reconnection_intensity :: integer(),
        has_pending_notifications = false
    }).


%%========================================================
%% gen_mod callbacks
%%========================================================


start(Host, Opts) ->
    ChildSpec = {?MODULE, {?MODULE, start_link, [Host, Opts]},
     transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    catch ?GEN_SERVER:call(?MODULE, stop),
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    ok.


%%====================================================================
%% hook callbacks
%%====================================================================

on_user_going_offline(User, Server, _Resource, _Status) ->
    case get_udid_for_user(User, Server) of
        {ok, null} ->
            ?INFO_MSG(" Got no token for user ~p ", [User]),
            ok;
        {ok, Udid} ->
            ?INFO_MSG(" Got token ~p for user ~p ", [Udid, User]),
            case catch gen_server:call(?MODULE, {add_user_to_notification_list, User, Udid}) of
                {'EXIT', {timeout, _}} ->
                    ok;
                _ ->
                    ok
            end;
        _ -> 
            ok
    end,
    ok.


on_user_coming_online(#jid{luser = User, lserver = Server} = JID) ->
    case catch gen_server:call(?MODULE, {remove_user_from_notification_list, User}) of
        {'EXIT', {timeout, _}} -> 
            ok;
        _ ->
            ok
    end.


%%====================================================================
%% gen_server callbacks
%%====================================================================

start_link(Host, Opts) ->
    ?GEN_SERVER:start_link({local, ?MODULE}, ?MODULE,
                           [Host, Opts], []).

init([Host, Opts]) ->
  ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, handle_push_notification, 50),
  ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, on_user_going_offline, 50),
  ejabberd_hooks:add(user_available_hook, Host, ?MODULE, on_user_coming_online, 50),
  ReconnectionIntensity = gen_mod:get_opt(reconnection_intensity, Opts, fun(A) -> A end, false),
  ApnsConnectionName = apple_connection, 
    State = #state{
        host = Host,
        apns_connection_name = ApnsConnectionName,
        opts = Opts
    },
    timer:apply_after(1000, ?MODULE, start_connection, [State]),
   {ok, State}.


terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, handle_push_notification, 50),
    ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, on_user_going_offline, 50),
    ejabberd_hooks:delete(user_available_hook, Host, ?MODULE, on_user_coming_online, 50),
    ok.


%%====================================================================
%% gen_server callbacks
%%====================================================================


handle_cast(_Info, State) -> 
    {noreply, State}.

handle_call({notify, Message, #jid{luser = User}} = JID,_From, #state{is_connected = false} = State) -> 
    #state{
        ios_offline_users = IosOfflineUsers, 
        apns_connection_name = ApnsConnectionName, 
        notification_queue = NotificationQueue
    } = State,
    case (?DICT):find(User, IosOfflineUsers) of
        error ->
            ?INFO_MSG(" Got no saved token for user ~p while queuing ", [User]),
            NewState = State;            
        {ok, DeviceToken} ->
            ?INFO_MSG(" Got  saved token ~p for user ~p while queuing ", [DeviceToken, User]),
            NewState = State#state{
                notification_queue = queue:in({Message, DeviceToken}, NotificationQueue), 
                has_pending_notifications = true
            }
    end,
    {noreply, NewState};

handle_call(
        {notify, Message, #jid{luser = User}},
        _From, 
        #state{has_pending_notifications = PStatus} = State
    ) -> 
    
    case PStatus of
        true ->
            flush_queue(State);
        _ ->
            ok
    end,
    #state{ios_offline_users = IosOfflineUsers, apns_connection_name = ApnsConnectionName} = State,
    case (?DICT):find(User, IosOfflineUsers) of
        error ->
            ?INFO_MSG(" Got no saved token for user ~p while sending message ", [User]),
            ok;            
        {ok, DeviceToken} ->
            ?INFO_MSG(" Got  saved token ~p for user ~p while sending message ", [DeviceToken, User]),
            send_message(ApnsConnectionName, DeviceToken, Message)
    end,
    {noreply, State#state{notification_queue = queue:new(), has_pending_notifications = false}};


handle_call({add_user_to_notification_list, User, IosDeviceId}, _From, State) ->
    #state{ios_offline_users = IosOfflineUsers} = State,
    AlteredDict = (?DICT):erase(User, IosOfflineUsers),
    NewDict = (?DICT):store(User, IosDeviceId, AlteredDict),
    NewState = State#state{ios_offline_users = NewDict},    
    {noreply, NewState};

handle_call({remove_user_from_notification_list, User}, _From, State) ->
    #state{ios_offline_users = IosOfflineUsers} = State,
    AlteredDict = (?DICT):erase(User, IosOfflineUsers),
    NewState = State#state{ios_offline_users = AlteredDict},    
    {noreply, NewState};

handle_call(start_connection, _From, State) -> 
    spawn(?MODULE, start_connection, [State]),
    {noreply, State#state{is_connected = false}};

handle_call(connection_established, _From, State) -> 
    {noreply, State#state{is_connected = true}};

handle_call(stop, _From, State) -> 
  {stop, normal, ok, State}.

handle_info(_Info, State) ->
    ?ERROR_MSG("got unexpected info: ~p", [_Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% apns callback functions
%%====================================================================
handle_error(MsgId, shutdown) ->
    case catch gen_server:call(?MODULE, start_connection) of 
        {'EXIT', {timeout, _}} ->
            ok;
        _ ->
            ok
    end,
  ok;

handle_error(MsgId, Status) ->
    ?ERROR_MSG("error: ~p error in apns connection ~p~n", [MsgId, Status]),
    ok.

handle_app_deletion(_) ->
    ok.


%%====================================================================
%% server internal functions
%%====================================================================
send_message(ApnsConnectionName, DeviceToken, Message) ->
    apns:send_message(ApnsConnectionName, #apns_msg{device_token = DeviceToken, alert = Message}).

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

flush_queue(#state{notification_queue = NotificationQueue, apns_connection_name = ApnsConnectionName} = State) ->
    NotificationList = queue:to_list(NotificationQueue),
    lists:foreach(
        fun ({Message, DeviceToken}) -> 
            send_message(ApnsConnectionName, DeviceToken, Message)
        end,
        NotificationList
    ).

start_connection(#state{opts = Opts, apns_connection_name = ApnsConnectionName} = State) ->
    ?INFO_MSG(" ~n Starting apns connection ~n ", []),
    case apns:connect(
        ApnsConnectionName,
        get_apns_connection_info(Opts)
    ) of 
        {error, nxdomain} ->
            ?ERROR_MSG(" Got nxdomain error in mod_apple_push ", []),
            #state{reconnection_intensity = ReconnectionIntensity} = State, 
            timer:apply_after(ReconnectionIntensity, ?MODULE, start_connection,
                [State]);
        {error, Reason} ->
            ?INFO_MSG(" ~n Error starting connection ~n  stopping server", [Reason]),
            gen_server:call(?MODULE, stop);
        Status ->
            ?INFO_MSG(" ~n Started apns connection ~n ", []),
            gen_server:call(?MODULE, connection_established),
            Status
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
    case catch gen_server:call(?MODULE, {notify, Message, To}) of 
        {'EXIT', {timeout, _}} -> 
            ok;
        _ ->
            ok
    end.

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
mod_opt_type(reconnection_intensity) ->
    fun (A) when is_integer(A) andalso A >= 0 -> A end;
mod_opt_type(cert_file) -> fun binary_to_list/1;
mod_opt_type(cert_password) -> fun binary_to_list/1;

mod_opt_type(_) -> [apple_host, apple_port, cert_file, cert_password, reconnection_intensity].

notify() ->
    ?ERROR_MSG("Problems in mod_apple_push", []).