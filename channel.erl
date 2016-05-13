-module(channel).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").
%%%%%%%
%%Join%%
%%%%%%%%
loop(St, {join, _Pid}) -> %% when someone tries to join a channel
    case (lists:member(_Pid, St#channel_st.channel_users)) of
        true -> {error, St}; %%User is already in the channel, return error
        false -> New_ch_st = St#channel_st{channel_users = St#channel_st.channel_users++[_Pid]},
                 {ok, New_ch_st}; %%User not in the channel, add to list as Pid
        undefined -> {error, St} %catch unexpected errors
    end;
%%%%%%
%%leave
%%%%%%%
loop(St, {leave, _Pid}) ->
    case (lists:member(_Pid, St#channel_st.channel_users)) of %%checks if the users PID exists in the list channel_users
        false -> {error, user_not_in_channel}; %%return an error if it doesn't
        true -> New_ch_st = St#channel_st{channel_users = St#channel_st.channel_users--[_Pid]}, %%otherwise remove the PID and update list
                {ok, New_ch_st}
    end;

%%%%%%%
%%%msg
%%%%%%%
loop(St, {msg, Nick, _Pid, _Msg}) -> %%uses _Pid to remove yourself from the list
    %%requests "incoming message" to be used for each PID in the channel, except yourself
    _=1+1,
    _=1+1,
    lists:foreach(
      fun(PID) ->
              spawn(fun() -> genserver:request(PID, {St#channel_st.name, Nick, _Msg}) end) end, St#channel_st.channel_users--[_Pid]),
    {ok, St}.

%%%%%%%%%
%%%state%%
%%%%%%%%%%
initial_state(_Channel, Pid) ->
    #channel_st{name = _Channel, channel_users=[Pid]}. %%users is a list of Nick,Pid
