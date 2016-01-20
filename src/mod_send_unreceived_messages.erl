%%%----------------------------------------------------------------------
%%% File    : mod_stanza_ack.erl
%%% Author  : Satish Chandra <satishck1992@gmaiil.com>
%%% Purpose : Saving unreceived messages 
%%% Usage   : Add the following line in modules section of ejabberd.cfg:
%%%              mod_send_unreceived_messages: {}  
%%%
%%%----------------------------------------------------------------------

-module(mod_send_unreceived_messages).

-behaviour(gen_mod).


-include("logger.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_unreceived_message.hrl").
-include("ejabberd_c2s.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1]).
-export([
    send_user_unreceived_messages/1, 
    handle_unreceived_messages/5, 
    delete_saved_unreceived_messages/2, 
    delete_all_saved_unreceived_messages/2
  ]).

start(Host, Opts) ->
  init_db(),

  ejabberd_hooks:add(user_available_hook, Host, ?MODULE, send_user_unreceived_messages, 10),
  ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, handle_unreceived_messages, 10),
  ejabberd_hooks:add(user_packet_confirmation_hook, Host, ?MODULE, delete_saved_unreceived_messages, 10),
  ejabberd_hooks:add(user_session_terminate_hook, Host, ?MODULE, delete_all_saved_unreceived_messages, 10),
  ok.

stop(Host) ->
  ejabberd_hooks:delete(user_available_hook, Host, ?MODULE, send_user_unreceived_messages, 10),
  ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, handle_unreceived_messages, 10),
  ejabberd_hooks:delete(user_packet_confirmation_hook, Host, ?MODULE, delete_saved_unreceived_messages, 10),
  ejabberd_hooks:delete(user_session_terminate_hook, Host, ?MODULE, delete_all_saved_unreceived_messages, 10),
  ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

init_db() ->
  mnesia:create_table(unreceived_message,
      [{disc_copies, [node()]}, {type, bag},
      {attributes, record_info(fields, unreceived_message)}]).


send_user_unreceived_messages(JID) ->
  User = JID#jid.luser,
  F = fun () ->
    Rs = mnesia:wread({unreceived_message, User}),
    mnesia:delete({unreceived_message, User}),
    Rs
  end,
    case mnesia:transaction(F) of
      {atomic, UnreceivedMessages} ->
          lists:foreach(
            fun(#unreceived_message.packet = El) ->
                      #xmlel{attrs = Attrs} = El,
                  From_s = xml:get_attr_s(<<"from">>, Attrs),
                  From = jlib:string_to_jid(From_s),
                  To_s = xml:get_attr_s(<<"to">>, Attrs),
                  To = jlib:string_to_jid(To_s),
                  ejabberd_router:route(From, To, El)
                end,
            lists:keysort(#unreceived_message.timestamp, UnreceivedMessages)
          ),
      ?INFO_MSG(" Sent lots of messages ", []);
      _ -> ok
    end,
    ok.

handle_unreceived_messages(Pkt, StateData, _JID, _Peer, _To) ->
    case should_save_message(Pkt) of
        true ->
          save_unreceived_messages(Pkt, StateData);
        _ -> 
          ok
    end,
    Pkt.

save_unreceived_messages(Message, StateData) ->
  AwayMessage = #unreceived_message{
    user = StateData#state.user,
    packet = Message,
    h_count = StateData#state.mgmt_stanzas_out, 
    timestamp = erlang:timestamp()
  },
  F = fun() ->
      mnesia:write(AwayMessage)
  end,
  Result = mnesia:transaction(F),
  case Result of
    {atomic, _} -> 
      ok;
    _ ->
      ?INFO_MSG(" ~n Message couldn't be saved, and no failback specified ~n ", []),
      ok
  end.

delete_saved_unreceived_messages(StateData, HCount) ->
  User = StateData#state.user,
    F = fun () ->
        case mnesia:select(
          unreceived_message,
            [
              {
              #unreceived_message{user = User, h_count = '$1', _ = '_'},
          [
            {'=<', '$1', HCount}
          ],
          ['$_']
          }
        ]
       ) of
          Messages when is_list(Messages) -> 
          lists:foreach(
            fun(Message) -> 
              mnesia:delete_object(Message)
            end,
            Messages
          );
          _ -> ok
        end
    end,
    Result = mnesia:transaction(F),
    case Result of
      {atomic, _} ->    
        ok;
      _ -> 
        ?INFO_MSG(" ~n Message couldn't be deleted, and no failback specified ~n ", [])
    end.

delete_all_saved_unreceived_messages(StateData, Reason) when Reason == normal ->
  US = {StateData#state.user, StateData#state.server},
  Q = fun() ->
    mnesia:delete(unreceived_message, US, write)
  end,
  case mnesia:transaction(Q) of
    {atomic, _} -> 
      ok;
    _ ->
      ?INFO_MSG(" ~n Message couldn't be saved, and no failback specified ~n ", []),
      ok
  end.

should_save_message(#xmlel{name = <<"message">>} = Packet) ->
    case {xml:get_attr_s(<<"type">>, Packet#xmlel.attrs),
          xml:get_subtag_cdata(Packet, <<"body">>)} of
        {<<"error">>, _} ->
            false;
        {_, <<>>} ->
            %% Empty body
            false;
        _ ->
            true
    end;
should_save_message(#xmlel{}) ->
    false.

