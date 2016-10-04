%%%----------------------------------------------------------------------
%%% File    : mod_ping.erl
%%% Author  : Brian Cully <bjc@kublai.com>
%%% Purpose : Support XEP-0199 XMPP Ping and periodic keepalives
%%% Created : 11 Jul 2009 by Brian Cully <bjc@kublai.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_ping).

-author('bjc@kublai.com').

-protocol({xep, 199, '2.0'}).

-behavior(gen_mod).

-behavior(gen_server).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-define(SUPERVISOR, ejabberd_sup).

-define(DEFAULT_SEND_PINGS, false).

-define(DEFAULT_PING_INTERVAL, 60).

%% API
-export([start_link/2, start_ping_listen/2, stop_ping_listen/2]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).

-export([iq_ping/3, user_online/2, user_offline/2,
	 user_send/4, mod_opt_type/1, depends/2]).

-record(state,
	{host = <<"">>,
         send_pings = ?DEFAULT_SEND_PINGS :: boolean(),
	 ping_interval = ?DEFAULT_PING_INTERVAL :: non_neg_integer(),
	 ping_ack_timeout = undefined :: non_neg_integer(),
	 timeout_action = none :: none | kill,
         timers = maps:new() :: map()}).

%%====================================================================
%% API
%%====================================================================
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

start_ping_listen(Host, JID) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {start_ping_listen, JID}).

stop_ping_listen(Host, JID) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {stop_ping_listen, JID}).

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    PingSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		transient, 2000, worker, [?MODULE]},
    supervisor:start_child(?SUPERVISOR, PingSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:delete_child(?SUPERVISOR, Proc).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    SendPings = gen_mod:get_opt(send_pings, Opts,
                                fun(B) when is_boolean(B) -> B end,
				?DEFAULT_SEND_PINGS),
    PingInterval = gen_mod:get_opt(ping_interval, Opts,
                                   fun(I) when is_integer(I), I>0 -> I end,
				   ?DEFAULT_PING_INTERVAL),
    PingAckTimeout = gen_mod:get_opt(ping_ack_timeout, Opts,
                                     fun(I) when is_integer(I), I>0 -> I * 1000 end,
                                     undefined),
    TimeoutAction = gen_mod:get_opt(timeout_action, Opts,
                                    fun(none) -> none;
                                       (kill) -> kill
                                    end, none),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             no_queue),
    mod_disco:register_feature(Host, ?NS_PING),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_PING, ?MODULE, iq_ping, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_PING, ?MODULE, iq_ping, IQDisc),
	ejabberd_hooks:add(status_online_hook, Host,
		     ?MODULE, user_online, 100),
	ejabberd_hooks:add(status_offline_hook, Host,
		     ?MODULE, user_offline, 100),
	ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		     user_send, 100),
    {ok,
     #state{host = Host, send_pings = SendPings,
	    ping_interval = PingInterval,
	    timeout_action = TimeoutAction,
	    ping_ack_timeout = PingAckTimeout,
	    timers = maps:new()}}.

terminate(_Reason, #state{host = Host}) ->
    ejabberd_hooks:delete(status_offline_hook, Host,
			  ?MODULE, user_offline, 100),
    ejabberd_hooks:delete(status_online_hook, Host,
			  ?MODULE, user_online, 100),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  user_send, 100),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_PING),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_PING),
    mod_disco:unregister_feature(Host, ?NS_PING).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({start_ping_listen, JID}, State) ->
    Timers = add_timer(JID, State#state.ping_interval,
		       State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast({stop_ping_listen, JID}, State) ->
    Timers = del_timer(JID, State#state.timers),
    {noreply, State#state{timers = Timers}};
% handle_cast({iq_pong, JID, timeout}, State) ->
%     Timers = del_timer(JID, State#state.timers),
%     ejabberd_hooks:run(user_ping_timeout, State#state.host,
% 		       [JID]),
%     case State#state.timeout_action of
%       kill ->
% 	  #jid{user = User, server = Server,
% 	       resource = Resource} =
% 	      JID,
% 	  case ejabberd_sm:get_session_pid(User, Server, Resource)
% 	      of
% 	    Pid when is_pid(Pid) -> ejabberd_c2s:close(Pid);
% 	    _ -> ok
% 	  end;
%       _ -> ok
%     end,
%     {noreply, State#state{timers = Timers}};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({timeout, _TRef, {ping, JID}}, State) ->
	#jid{user = User, server = Server,
	       resource = Resource} =
	      JID,
  	case ejabberd_sm:get_session_pid(User, Server, Resource)
	      of
	    Pid when is_pid(Pid) -> 
            ejabberd_c2s:set_unavailable(Pid);
	    _ -> ok
	end,
	Timers = del_timer(JID, State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% Hook callbacks
%%====================================================================
iq_ping(_From, _To,
	#iq{type = Type, sub_el = SubEl, lang = Lang} = IQ) ->
    case {Type, SubEl} of
      {get, #xmlel{name = <<"ping">>}} ->
	  IQ#iq{type = result, sub_el = []};
      _ ->
	  Txt = <<"Ping query is incorrect">>,
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERRT_BAD_REQUEST(Lang, Txt)]}
    end.

user_online(JID, Server) ->
    start_ping_listen(JID#jid.lserver, JID),
    ok.

user_offline(JID, Server) ->
    stop_ping_listen(JID#jid.lserver, JID),
    ok.

user_send(Packet, _C2SState, JID, _From) ->
    #xmlel{name = Name, attrs = Attrs} = Packet,
    case Name of 
    	<<"message">> ->
		    start_ping_listen(JID#jid.lserver, JID);
		_ ->
			ok
	end,
    Packet.

%%====================================================================
%% Internal functions
%%====================================================================
add_timer(JID, Interval, Timers) ->
     LJID = jlib:jid_tolower(JID),
    NewTimers = case maps:find(LJID, Timers) of
      {ok, OldTRef} ->
		      cancel_timer(OldTRef),
          maps:remove(LJID, Timers);
      _ -> Timers
		end,
    TRef = erlang:start_timer(Interval * 1000, self(),
			      {ping, JID}),
    maps:put(LJID, TRef, NewTimers).

del_timer(JID, Timers) ->
     LJID = jlib:jid_tolower(JID),
    case maps:find(LJID, Timers) of
      {ok, TRef} ->
	  cancel_timer(TRef),
    maps:remove(LJID, Timers);
      _ -> Timers
    end.

cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
      false ->
	  receive {timeout, TRef, _} -> ok after 0 -> ok end;
      _ -> ok
    end.

depends(_Host, _Opts) ->
    [].

mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(ping_interval) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(ping_ack_timeout) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(send_pings) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(timeout_action) ->
    fun (none) -> none;
	(kill) -> kill
    end;
mod_opt_type(_) ->
    [iqdisc, ping_interval, ping_ack_timeout, send_pings, timeout_action].
