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

-define(PROCNAME, ejabberd_apns).


-record(state,{host = <<"">> : binary(),
		apns_connection_name :: atom()
	}).

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ?GEN_SERVER:start_link({local, Proc}, ?MODULE,
                           [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    catch ?GEN_SERVER:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, Opts]) ->
	ApnsConnectionName = apple_connection, 
	ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, on_offline_user_recieving_message, 50),
	ApnsConnectionInfo = #apns_connection{
		apple_host        = gen_mod:get_opt(apple_host),
        apple_port        = gen_mod:get_opt(apple_port),
      	cert              = gen_mod:get_opt(cert),
      	cert_file         = gen_mod:get_opt(cert_file),
      	key               = gen_mod:get_opt(key),
      	key_file          = gen_mod:get_opt(key_file),
      	cert_password     = gen_mod:get_opt(cert_password),
      	timeout           = gen_mod:get_opt(timeout),
      	error_fun         = gen_mod:get_opt(error_fun),
      	feedback_host     = gen_mod:get_opt(feedback_host),
      	feedback_port     = gen_mod:get_opt(feedback_port),
      	feedback_fun      = gen_mod:get_opt(feedback_fun),
      	feedback_timeout  = gen_mod:get_opt(feedback_timeout),
      	expires_conn      = gen_mod:get_opt(expires_conn),
      	extra_ssl_opts    = gen_mod:get_opt(extra_ssl_opts),
      	error_logger_fun  = gen_mod:get_opt(error_logger_fun),
      	info_logger_fun   = gen_mod:get_opt(info_logger_fun)
    },
    case apns:connect(
    	ApnsConnectionName,
    	fun ?MODULE:handle_error/2,
    	fun ?MODULE:handle_app_deletion/1 
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


handle_cast(_Msg, State) -> {noreply, State}.


handle_call(stop, _From, State) -> {stop, normal, ok, State}.


handle_info(#apns_msg{} = Message, State) ->
	apns:send_message(State#state.apns_connection_name, Message),
    {noreply, State};

handle_info(_Info, State) ->
    ?ERROR_MSG("got unexpected info: ~p", [_Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% gen_server internal functions
%%====================================================================


on_offline_user_recieving_message(From, To, Packet) ->
	User = To#jid.luser,
	Server = To#jid.lserver, 
	case get_udid_for_user(User, Server) of
		{ok, Udid} ->
			handle_push_notification(To, Packet, Udid),
			ok;
		_ -> 
			ok
	end,
	Packet.

get_udid_for_user(User, Server) ->
	case ejabberd_odbc:sql_query(
	   Server,
	   [<<"select udid from users " >>, 
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
 handle_push_notification(To, Message, Udid) ->
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
 					notify(To, Message, Udid)
 			end;
 		_ ->
 			ok
 	end.

notify(To, Message, Udid) ->
	gen_mod:get_module_proc(To#jid.lserver, ?PROCNAME) !
	#apns_msg{
		alert = Message,
		device_token = Udid 
	}.