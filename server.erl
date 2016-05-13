-module(server).
-export([loop/2, initial_state/1]).
 
-include_lib("./defs.hrl").
%%Connect%%
loop(St, {connect, Nick, Pid}) ->
    case (lists:member(Pid, St#server_st.users)) of %%checks if user PID is present in server user list
        true -> {already_connected, St}; %%PID was present, already connected
        false -> 
            case lists:member(Nick, St#server_st.list_of_nicks) of %%checks if nick is available
                true -> {nick_in_use, St}; %%someone had already taken that nickname
                false -> New_St = St#server_st{users = St#server_st.users++[Pid], %%add pid + nick to server lists
                                               list_of_nicks = St#server_st.list_of_nicks++[Nick]},
                         {ok, New_St}
            end
    end;

%%Disconnect%%
loop(St, {disconnect, Nick, Pid}) ->
    case (lists:member(Pid, St#server_st.users)) of %%check if PID exists in user list
        true -> New_St = St#server_st{users=St#server_st.users--[Pid], %%remove users PID and NICK from the server list
                                      list_of_nicks = St#server_st.list_of_nicks--[Nick]},
                 {ok,New_St};
        false -> {user_not_connected, St} %%PID did not exist, user not connected to this server
    end;
 
%%Join Channel%%
loop(St, {join, Pid, Channel}) ->
    case (lists:member(Channel,St#server_st.channels)) of %%checks if channel exists on the server already
        true ->
            Join = genserver:request(list_to_atom(Channel),{join,Pid}), %%channel exists, attempt to join
            case Join of
                ok -> {ok, St};
                error -> {user_already_in_channel, St}
            end;
        false -> %%channel did not exist, attempt to start a channel process with set name Channel.
            genserver:start(list_to_atom(Channel), channel:initial_state(Channel,Pid),fun channel:loop/2),
						%%Contacts channel and starts a process. Automatically add user PID in channel when created
            New_St = St#server_st{channels = St#server_st.channels++[Channel]},
            {ok, New_St}
    end;
 
%%
loop(St, _Msg) ->
    {ok, St}.
 
%%Initial State%%
initial_state(_Server) ->
    #server_st{name = _Server, users=[], channels=[], list_of_nicks=[]}.
