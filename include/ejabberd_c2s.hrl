%% This record is represents the c2s state
%% This has been done to remove code duplicacy 
%% as we may need the state record elsewhere. 


-ifdef(SETS).
 ok.
-else.
-define(SETS, gb_sets).
-endif.

-ifdef(DICT).
 ok.
-else.
-define(DICT, dict).
-endif.

-include("mod_privacy.hrl").

-record(state, {socket,
		sockmod,
		socket_monitor,
		xml_socket,
		streamid,
		sasl_state,
		access,
		shaper,
		zlib = false,
		tls = false,
		tls_required = false,
		tls_enabled = false,
		tls_options = [],
		authenticated = false,
		jid,
		user = <<"">>, server = <<"">>, resource = <<"">>,
		sid,
		pres_t = ?SETS:new(),
		pres_f = ?SETS:new(),
		pres_a = ?SETS:new(),
		pres_last,
		pres_timestamp,
		privacy_list = #userlist{},
		conn = unknown,
		auth_module = unknown,
		ip,
		aux_fields = [],
		csi_state = active,
		csi_queue = [],
		mgmt_state,
		mgmt_xmlns,
		mgmt_queue,
		mgmt_max_queue,
		mgmt_pending_since,
		mgmt_timeout,
		mgmt_resend,
		mgmt_stanzas_in = 0,
		mgmt_stanzas_out = 0,
		lang = <<"">>,
		is_available = true,
		has_saved_messages = true,
		apple_udid}).
