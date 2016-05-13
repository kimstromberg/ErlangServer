-module(client).
-export([loop/2, initial_state/2]).
 
-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%%%%% Connect_remote
%%%%%%%%%%%%%%%%%%
loop(St, {connect, {_Server, _Machine}}) ->
    case St#cl_st.connected_server == [] of
        false ->
            {{error, user_already_connected, "User is already connected"},St};
        true ->
            Connection = (catch(genserver:request({list_to_atom(_Server), list_to_atom(_Machine)}, {connect, St#cl_st.nick, self()}))),
            case Connection of
                ok -> New_St = St#cl_st{connected_server = {list_to_atom(_Server), list_to_atom(_Machine)},
                                        machine = list_to_atom(_Machine)},
                      {ok,New_St};
                {'EXIT',Reason} -> {{error, server_not_reached, "Server could not be reached: " ++ Reason}, St};
                already_connected -> {{error, user_already_connected, "User is already connected"}, St};
                nick_in_use -> {{error, user_already_connected, "Nick is taken"}, St}
            end
    end;

%%%%%%%%%%%%%%%
%%%%%%% Connect
%%%%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
    case St#cl_st.connected_server == [] of
        false ->
            {{error, user_already_connected, "User is already connected"},St};
        true ->
            case whereis(list_to_atom(_Server)) of %%check if there is a process with this name running
                undefined -> {{error, server_not_reached, "Server not reached"}, St}; %%none, found. Incorrect server then
                _ ->	%%process found. Proceed to try and connect to it
                    Connection = genserver:request(list_to_atom(_Server), {connect, St#cl_st.nick, self()}),
                    case Connection of
                        ok -> New_St = St#cl_st{connected_server = list_to_atom(_Server)},
                              {ok,New_St};
                        already_connected -> {{error, user_already_connected, "User is already connected"}, St};
                        nick_in_use -> {{error, user_already_connected, "Nick is taken"}, St}
                    end
            end
    end;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
loop(St, disconnect) ->
    case (St#cl_st.connected_channels == []) of %%checks if user is in any channels
        true ->
            case St#cl_st.connected_server == [] of %%checks if user is connected to a server
                false -> (catch(genserver:request(list_to_atom(St#cl_st.connected_server),{disconnect, St#cl_st.nick,self()}))),
                        New_St = St#cl_st{connected_server = ""}, %% removes the connected server from the client list
                        {ok, New_St};
                true -> {{error, user_not_connected, "User not connected"}, St}
            end;
        false -> {{error, leave_channels_first ,"User can't leave while in channels"}, St}
    end;
%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join, _Channel}) ->
    case lists:member(_Channel, St#cl_st.connected_channels) of
        false ->
            genserver:request(St#cl_st.connected_server, {join, self(), _Channel}),
            New_St = St#cl_st{connected_channels = St#cl_st.connected_channels ++ [_Channel]},
            {ok,New_St};
        true -> {{error, user_already_joined, "User is already in channel " ++ _Channel}, St}
    end;
 
%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
    case lists:member(_Channel, St#cl_st.connected_channels) of %%checks if user is in the channel
        true ->
            case St#cl_st.machine == "" of
                false ->	genserver:request({list_to_atom(_Channel), St#cl_st.machine}, {leave, self()}); %%contacts channel if yes
                true ->  	genserver:request(list_to_atom(_Channel), {leave, self()}) %%contacts channel if yes
            end,
            New_St = St#cl_st{connected_channels = St#cl_st.connected_channels--[_Channel]},%%remove user from client channel list
            {ok, New_St};
        false -> {{error, user_not_joined, "User is not in channel " ++ _Channel}, St};
        undefined -> {{error, server_not_reached, "Server not reached"}, St} %% catch unexpected errors
    end;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
    case lists:member(_Channel, St#cl_st.connected_channels) of %%checks if user is in the channel
        true ->
            case St#cl_st.machine == "" of
                false -> genserver:request({list_to_atom(_Channel), St#cl_st.machine}, {msg, St#cl_st.nick, self(), _Msg});
                true -> genserver:request(list_to_atom(_Channel), {msg, St#cl_st.nick, self(), _Msg})
            end,
            {ok,St};
        false -> {{error, user_not_joined, "User is not in channel"}, St}
    end;

%%%%%%%%%%%%%%
%%% WhoIam
%%%%%%%%%%%%%%
loop(St, whoiam) ->
    case St#cl_st.nick of
        null ->  {"user01",St};
        _Else -> {St#cl_st.nick,St}
    end;
 
%%%%%%%%%%
%%% Nick
%%%%%%%%%%
loop(St,{nick,_Nick}) ->
    New_St = St#cl_st{nick = _Nick},
    {ok, New_St};
 
%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%
loop(St, debug) ->
    {St, St} ;
 
%%%%%%%%%%%%%%%%%%%%%
%%%% Incoming message
%%%%%%%%%%%%%%%%%%%%%
loop(St = #cl_st { gui = GUIName }, _MsgFromClient) ->
    {Channel, Name, Msg} = decompose_msg(_MsgFromClient),
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.
 
% This function will take a message from the client and
% decomposed in the parts needed to tell the GUI to display
% it in the right chat room.
decompose_msg(_MsgFromClient) ->
    _MsgFromClient. %%Made the channel return the message in a correct tuple already
 
%%
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, connected_server = "", connected_channels = [], machine = ""}.
