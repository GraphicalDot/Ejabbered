%%%----------------------------------------------------------------------
%%% File    : mod_stanza_ack.erl
%%% Author  : Kay Tsar <kay@mingism.com>
%%% Purpose : Message Receipts XEP-0184 0.5
%%% Created : 25 May 2013 by Kay Tsar <kay@mingism.com>
%%% Usage   : Add the following line in modules section of ejabberd.cfg:
%%%              {mod_stanza_ack,  [{host, "zilan"}]}
%%%
%%%
%%% Copyright (C) 2013-The End of Time   Mingism
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_stanza_ack).

-behaviour(gen_mod).
-define(GEN_SERVER, p1_server).
-behaviour(?GEN_SERVER).

-include("logger.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").

-type host()    :: string().
-type name()    :: string().
-type value()   :: string().
-type opts()    :: [{name(), value()}, ...].

-define(NS_RECEIPTS, <<"urn:xmpp:receipts">>).
-define(EJABBERD_DEBUG, true).

-record(state, {
    included_hosts :: [binary(),...],
    acknowledging_id :: binary()    
    }).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1]).
-export([on_user_send_packet/4]).
-export([start_link/2, init/1, handle_call/3, handle_cast/2,
   handle_info/2, terminate/2, code_change/3,
   mod_opt_type/1]).

-spec start(host(), opts()) -> ok.
start(Host, Opts) ->
	mod_disco:register_feature(Host, ?NS_RECEIPTS),
	ejabberd_hooks:add(user_send_packet, Host, ?MODULE, on_user_send_packet, 10),
    ChildSpec = {?MODULE, {?MODULE, start_link, [Host, Opts]},
     transient, 1000, worker, [?MODULE]},
  supervisor:start_child(ejabberd_sup, ChildSpec).

-spec stop(host()) -> ok.
stop(Host) ->
	ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, on_user_send_packet, 10),
    catch ?GEN_SERVER:call(?MODULE, stop),
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
	ok.

%% ====================================================================
%% gen_server internal functions
%% ====================================================================

start_link(Host, Opts) ->
    ?GEN_SERVER:start_link({local, ?MODULE}, ?MODULE,
                           [Host, Opts], []).

init([Host, Opts]) ->
    IncludedHosts = gen_mod:get_opt(included_hosts, Opts, fun(A) when is_list(A) ->  A end, false),
    AcknowledgingId = gen_mod:get_opt(acknowledging_id, Opts, fun(A) when is_binary(A) ->  A end, false),
    {ok, #state{ included_hosts = IncludedHosts, acknowledging_id = AcknowledgingId}}.

handle_info(_Info, State) ->
    ?ERROR_MSG("got unexpected info: ~p", [_Info]),
    {noreply, State}.
handle_cast(_Info, State) ->
    ?ERROR_MSG("got unexpected info: ~p", [_Info]),
    {noreply, State}.

handle_call({acknowledge, Packet, From, To}, _From, State) -> 
    case should_acknowledge(Packet, State, To) of 
        S when (S==true) ->
            RegisterToJid = From, %used in ack stanza
            send_ack_response(From, To, Packet, State#state.acknowledging_id);
        false ->
            ok
    end,
    {noreply, State};

handle_call(stop, _From, State) -> 
  {stop, normal, ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

on_user_send_packet(Packet, _C2SState, From, To) ->
    case catch gen_server:call(?MODULE, {acknowledge, Packet, From, To}) of 
        {'EXIT', {timeout, _}} -> 
            ok;
        _ ->
            ok
    end,
    Packet.

should_acknowledge(#xmlel{name = <<"message">>} = Packet, #state{included_hosts = IncludedHosts} = State, #jid{lserver = LServer}  = To) ->
    case {xml:get_attr_s(<<"type">>, Packet#xmlel.attrs),
          xml:get_subtag_cdata(Packet, <<"body">>),
          LServer} of
        {<<"error">>, _, _} ->
            false;
        {_, <<>>, _} ->
            %% Empty body
            false;
        _ ->
            lists:member(LServer, IncludedHosts) 
    end;
should_acknowledge(_, _, _) ->
    false.

send_ack_response(From, To, Packet, AcknowledgingId) ->
    ReceiptId = xml:get_tag_attr_s(<<"id">>, Packet),
    SentTo = jlib:jid_to_string(To),
    XmlBody = #xmlel{name = <<"message">>,
              		    attrs = [{<<"from">>, From}, {<<"to">>, To}],
              		    children =
              			[#xmlel{name = <<"received">>,
              				attrs = [{<<"xmlns">>, ?NS_RECEIPTS}, {<<"id">>, ReceiptId}, {<<"sent_to">>, SentTo}],
              				children = []}]},
    ejabberd_router:route(jlib:string_to_jid(AcknowledgingId), From, XmlBody).



mod_opt_type(included_hosts) ->
    fun (A) when is_list(A) -> A end;
mod_opt_type(acknowledging_id) ->
    fun (A) when is_binary(A) -> A end;
mod_opt_type(_) -> [included_hosts, acknowledging_id].
