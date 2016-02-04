-module(mod_apple_push).
-author("satishck1992@gmail.com").
-behaviour(gen_mod).

-define(GEN_SERVER, p1_server).
-behaviour(?GEN_SERVER).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include_lib("apns/include/apns.hrl").

-export([start/2, stop/1]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).


-export([handle_error/2, 
        handle_app_deletion/1]).


-define(DEFAULT_APPLE_HOST, "gateway.sandbox.push.apple.com").
-define(DEFAULT_APPLE_PORT, 2195).
-define(DEFAULT_CERT, undefined).
-define(DEFAULT_CERT_FILE, "priv/cert_file.pem").
-define(DEFAULT_KEY, undefined).
-define(DEFAULT_KEY_FILE, undefined).
-define(DEFAULT_CERT_PASSWORD, undefined).
-define(DEFAULT_TIMEOUT, 30000).
-define(DEFAULT_FEEDBACK_HOST, "feedback.sandbox.push.apple.com").
-define(DEFAULT_FEEDBACK_PORT, 2196).
-define(DEFAULT_FEEDBACK_TIMEOUT, 30*60*1000).
-define(DEFAULT_EXPIRES_CONN, 300).
-define(DEFAULT_EXTRA_SSL_OPTS, []).

-record(state,{host = <<"">> : binary(),
        apns_connection_name :: atom()
    }).


%%========================================================
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
%% gen_server callbacks
%%====================================================================

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ?GEN_SERVER:start_link({local, Proc}, ?MODULE,
                           [Host, Opts], []).

init([Host, Opts]) ->
  ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, on_offline_user_recieving_message, 50),
  ApnsConnectionName = apple_connection, 
    case apns:connect(
        ApnsConnectionName,
        get_apns_connection_info()
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
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, on_offline_user_recieving_message, 50),
    ok.


%%====================================================================
%% gen_server callbacks
%%====================================================================


handle_cast({notify, #apns_msg{} = Message}, State) -> 
    apns:send_message(State#state.apns_connection_name, Message),
    {noreply, State}.

handle_call(stop, _From, State) -> 
  {stop, normal, ok, State}.

handle_info(_Info, State) ->
    ?ERROR_MSG("got unexpected info: ~p", [_Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%====================================================================
%% apns callback functions
%%====================================================================
handle_error(_, _) ->
  ok.

handle_app_deletion(_) ->
  ok.


%%====================================================================
%% gen_server internal functions
%%====================================================================


on_offline_user_recieving_message(From, To, Packet) ->
    User = To#jid.luser,
    Server = To#jid.lserver, 
    case get_udid_for_user(User, Server) of
        {ok, Udid} ->
            handle_push_notification(To, Packet, Udid, Server),
            ok;
        _ -> 
            ok
    end,
    Packet.

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

%% Erlsom has been used for parsing the xml key 
%% value pair. Each sml tag is transformed to a tuple
%% of {Tag, Attributes, Content}
 handle_push_notification(To, Message, Udid, Server) ->
    case erlsom:simple_form(Message) of 
        {ok, {"message", _, Content}, Tail} -> 
            {_, {_, _, ContentList}} = lists:keysearch("properties", 1, Content),
            [MimeNameValue] = lists:filter(
                fun({_, _, NameValueList}) -> 
                    case lists:keysearch(["mime_type"], 3, NameValueList) of
                        {value, _} ->
                            true;
                        _ ->
                            false
                    end
                end, 
                ContentList
            ),
            {_, _, [MessageType]} = lists:keysearch("value", 1, MimeNameValue),
            Message = case MessageType of
                "t" ->
                    <<" You received a message ">>;
                "s" ->
                    <<" You received a sticker ">>;
                "i" ->
                    <<" You received an image ">>;
                "a" ->
                    <<" You received audio clip ">>;
                "v" ->
                    <<" You received video clip ">>;
                _ ->
                    error 
            end,
            case Message of 
                error -> 
                    ok;
                _ -> 
                    notify(To, Message, Udid, Server)
            end;
        _ ->
            ok
    end.

notify(To, Message, Udid, Host) ->
  Proc = gen_mod:get_module_proc(Host, ?MODULE),
  gen_server:cast(Proc, {notify, 
    #apns_msg{
        alert = Message,
        device_token = Udid 
    }
  }).


  get_apns_connection_info() -> 
      #apns_connection{
            apple_host        = gen_mod:get_opt(apple_host, fun(B) when is_binary(B) -> B end, 
                ?DEFAULT_APPLE_HOST),
            apple_port        = gen_mod:get_opt(apple_port, fun(B) when is_integer(B) -> B end, 
                ?DEFAULT_APPLE_PORT),
          	cert              = gen_mod:get_opt(cert, fun(B) when is_binary(B) -> B end, 
                ?DEFAULT_CERT),
          	cert_file         = gen_mod:get_opt(cert_file, fun(B) when is_binary(B) -> B end, 
                ?DEFAULT_CERT_FILE),
          	key               = gen_mod:get_opt(key, fun(B) when is_binary(B) -> B end, 
                ?DEFAULT_KEY),
          	key_file          = gen_mod:get_opt(key_file, fun(B) when is_binary(B) -> B end, 
                ?DEFAULT_KEY_FILE),
          	cert_password     = gen_mod:get_opt(cert_password, fun(B) when is_binary(B) -> B end, 
                ?DEFAULT_CERT_PASSWORD),
          	timeout           = gen_mod:get_opt(timeout, fun(B) when is_integer(B) -> B end, 
                ?DEFAULT_TIMEOUT),
          	feedback_host     = gen_mod:get_opt(feedback_host, fun(B) when is_binary(B) -> B end, 
                ?DEFAULT_FEEDBACK_HOST),
          	feedback_port     = gen_mod:get_opt(feedback_port, fun(B) when is_integer(B) -> B end, 
                ?DEFAULT_FEEDBACK_PORT),
          	feedback_timeout  = gen_mod:get_opt(feedback_timeout, fun(B) when is_integer(B) -> B end, 
                ?DEFAULT_FEEDBACK_TIMEOUT),
          	expires_conn      = gen_mod:get_opt(expires_conn, fun(B) when is_integer(B) -> B end, 
                ?DEFAULT_EXPIRES_CONN),
          	extra_ssl_opts    = gen_mod:get_opt(extra_ssl_opts, fun(B) when is_list(B) -> B end, 
                ?DEFAULT_EXTRA_SSL_OPTS),
            error_fun = fun ?MODULE:handle_error/2,
            feedback_fun = fun ?MODULE:handle_app_deletion/1
        }.