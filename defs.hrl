% This record defines the structure of the 
% client process. 
% 
% It contains the following fields: 
%
% gui: it stores the name (or Pid) of the GUI process.
%
-record(cl_st, {gui, nick, connected_server, connected_channels, machine}). %%TODO,Need list of channels here to 
    
% This record defines the structure of the 
% server process. 
% 
-record(server_st, {name, users, channels, list_of_nicks, clients}).

%Record for channels to keep track of users
-record(channel_st, {name, channel_users}). %% Channel name + a list of pids
