-module(mod_apple_push).
-author("satishck1992@gmail.com").
-behaviour(gen_mod).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-export([start/2, stop/1]).

start(Host, Opts) ->
	ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, on_offline_user_recieving_message, 50),
	ok.

stop(Host) -> 
	ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, on_offline_user_recieving_message, 50),
	ok.

on_offline_user_recieving_message(From, To, Packet) ->
	User = To#jid.luser,
	Server = To#jid.lserver, 
	case get_udid_for_user(User, Server) of
		{ok, Udid} ->
			% handle_push_notification(To, Packet);
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