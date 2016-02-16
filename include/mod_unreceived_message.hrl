%% This record is made to store the messages
%% which have been recieved by the user's process
%% but haven't been recieved by the user. This 
%% creates a problem when the server crashes.
%% Hence we store the messages until we can send them to the user. 
-record(unreceived_message,
	{	
		user = <<"">> :: binary(),
       	packet = #xmlel{}     :: xmlel() | '_',
       	h_count :: integer(),
       	timestamp =  erlang:timestamp() :: erlang:timestamp() | '_'
    }
).