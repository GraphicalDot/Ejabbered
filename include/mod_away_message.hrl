%% This record is made to store the messages
%% which have been recieved by the user's process
%% but haven't been recieved by the user. This 
%% creates a problem when the server crashes.
%% Hence we store the messages until we can send them to the user. 
-record(away_message,
	{	
		ush = {<<"">>, <<"">> , 0} :: {binary(), binary(), integer()},
       	packet = #xmlel{}     :: xmlel() | '_',
       	timestamp = now() :: erlang:timestamp() | '_'
    }
).