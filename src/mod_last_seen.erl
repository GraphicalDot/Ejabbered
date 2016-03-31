-module(mod_last_seen).

-author('satishck1992@gmail.com').

-behaviour(gen_mod).
-define(MYHOST, <<"dev.mm.io">>).

-define(EJABBERD_DEBUG, true).
-define(NS_RECEIPTS, <<"urn:xmpp:receipts">>).

-include("logger.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").

-type host()    :: string().
-type name()    :: string().
-type value()   :: string().
-type opts()    :: [{name(), value()}, ...].

-export([start/2, stop/1]).
-export([process/3]).

-spec start(host(), opts()) -> ok.
start(Host, Opts) ->
	ejabberd_router:register_route(?MYHOST, {apply, ?MODULE, process}),
	ok.

-spec stop(host()) -> ok.
stop(Host) ->
	ok.

process(From, To, Packet) ->
	case packet_type(Packet) of 
		S when (S==set) ->
			store_info(Packet, From),
			ok;
		S when (S==get) ->
			send_last_seen_info(Packet ,From),
			ok;
		_ ->
			ok
	end.

packet_type(Packet) -> 
	case {xml:get_tag_attr_s(<<"to">>, Packet)} of
	{<<"gettime@dev.mm.io">>} ->
		get;
	{<<"settime@dev.mm.io">>} ->
		set;
	_ ->
		false
	end.


send_last_seen_info(Packet, From) ->
    User = xml:get_subtag_cdata(Packet, <<"body">>),
	LServer = From#jid.lserver,
    case ejabberd_odbc:sql_query(
           LServer,
           [<<"select last_seen from users " >>, 
           		<<" where username  = ">>,
           		<<"'">>, User, <<"';">>]) of

        {selected, _, [[Timestamp]]} -> 
        	send_response(From, Timestamp, Packet, User);
        _ ->
          ok
    end.	

send_response(To, Timestamp, Packet, User) ->
    RegisterFromJid = <<"dev@dev.mm.io">>, %used in ack stanza
	  From = jlib:string_to_jid(RegisterFromJid),
    SentTo = jlib:jid_to_string(To),
    Type = xml:get_tag_attr_s(<<"type">>, Packet),

    XmlBody = #xmlel{name = <<"message">>,
              		    attrs = [{<<"from">>, RegisterFromJid}, {<<"to">>, To}, {<<"type">>, Type}],
              		    children =
              			[#xmlel{name = <<"body">>,
              				attrs = [],
              				children = [#xmlel{name = <<"jid">>,
                                  attrs = [],
                                  children = [{xmlcdata, User}]},
                                  #xmlel{name = <<"timestamp">>,
                                  attrs = [],
                                  children = [{xmlcdata, Timestamp}]}
                                  ]
                            }
                    ]
                    },
    ejabberd_router:route(From, To, XmlBody).

store_info(Packet, From) ->
    Username = From#jid.luser,
    LServer = From#jid.lserver,
    Timestamp = xml:get_subtag_cdata(Packet, <<"body">>),
    ejabberd_odbc:sql_query(LServer,[<<"update users set last_seen = ">>,
    								<<"'">>, Timestamp, <<"'">>,
    								<<" where username='">>,
    								Username, <<"';">>]).
