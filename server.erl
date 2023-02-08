%%%-------------------------------------------------------------------
%%% @author Pumpkin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(server).
-author("Pumpkin").
-behaviour(gen_server).

-export([start_link/1, s/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(server_state, {sock}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

s() ->
  start_link(?DEFAULT_PORT).

start_link(Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

init([Port]) ->
  ets:new(id, [ordered_set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
  case gen_tcp:listen(Port, [binary, {packet, 0}, {active, true}]) of
    {ok, ListenSocket} ->
      io:format("server begin~n"),
      spawn_link(fun() -> wait_connect(ListenSocket) end),
      {ok, #server_state{sock = ListenSocket}};
    {error, Reason} ->
      io:format("~p~n", [Reason]),
      {stop, Reason, #server_state{}}
  end.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({tcp_closed, Socket}, State) ->
  io:format("Server socket :~p closed ~n",[Socket]),
  {noreply, State};
handle_info({tcp, Socket, Bin}, State) ->
  io:format("socket :~p~n",[Socket]),
  [Id, Sign, PassWord, SendId, MessageInfos] = binary_to_term(Bin),
  if
    Sign =:= register_user ->
      Info = register_user(Id, PassWord, Socket),
      gen_tcp:send(Socket, term_to_binary(Info));
    Sign =:= login_user ->
      Info = login_user(Id, PassWord, Socket),
      gen_tcp:send(Socket, term_to_binary(Info));
    Sign =:= login_out ->
      Info = login_out(Id, Socket),
      gen_tcp:send(Socket, term_to_binary(Info));
    Sign =:= private_msg ->
      private_chat(SendId, Socket, MessageInfos);
    Sign =:= group_msg ->
      group_chat(Socket, MessageInfos);
    true ->
      io:format("error sign ~n")
  end,
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
wait_connect(ListenSocket) ->
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
      io:format("accept tcp ~p~n",[Socket]),
      gen_tcp:controlling_process(Socket, whereis(server)),
      wait_connect(ListenSocket);
    {error, Reason} ->
      io:format("accept tcp error,reason=~p~n",[Reason])
  end.

%% 用户注册
register_user(Id, PassWord, _Socket) ->
  case ets:lookup(id, Id) of
    [_Ok] ->
      io:format("~ts~n",[list_to_binary("账号已存在")]),
      "账号已存在";
    _ ->
      ets:insert(id, {Id, PassWord, 0, 0}),
      "注册成功，请进行登录操作"
  end.

%% 用户登录
login_user(Id, PassWord, Socket) ->
  case ets:match_object(id, {Id, PassWord, 0, 0}) of
    [_Ok] ->
      OnlineList = ets:match_object(id, {'_', '_', 1, '_'}),
      [gen_tcp:send(Socket, term_to_binary("用户"++integer_to_list(Id)++"加入了聊天室"))|| {_,_,_,Socket} <- OnlineList],
      ets:update_element(id, Id, [{3, 1}, {4, Socket}]),
      "登录成功!当前在线用户"++integer_to_list(length(OnlineList)+1)++"人";
    Reson ->
      io:format("login is fail ~n ~p", [Reson]),
      "密码错误或账号不存在"
  end.

%% 退出用户
login_out(Id, Socket) ->
  %% 因为id对应唯一socket，所以不需要PassWord
  case ets:match_object(id, {Id, '_', 1, Socket}) of
    [_Ok] ->
      ets:update_element(id, Id, [{3, 0}, {4, 0}]),
      OnlineList = ets:match_object(id, {'_', '_', 1, '_'}),
      [gen_tcp:send(Socket, term_to_binary("用户"++integer_to_list(Id)++"离开了聊天室"))|| {_,_,_,Socket} <- OnlineList],
      "注销成功";
    _ ->
      io:format("~ts~n",[list_to_binary("注销失败")]),
      "注销失败"
  end.

%% 群聊
group_chat(Socket, MessageInfos) ->
  case ets:match_object(id, {'_', '_', 1, Socket}) of
    [{Id, _, _, _}] ->
      Res = ets:match_object(id, {'_', '_', 1, '_'}),
      case Res =:= [] of
        true ->
          io:format("~ts~n",[list_to_binary("现在没有在线玩家")]);
        _ ->
          group_send_msg(Res, Id, MessageInfos)
      end;
    _ ->
      io:format("~ts~n",[list_to_binary("未登录，群聊失败")])
  end.


%% 群聊发送
group_send_msg([], _Id, _MessageInfos) ->
  next;
group_send_msg([Info | Infos], Id, MessageInfos) ->
  {_, _, _, Socket} = Info,
  gen_tcp:send(Socket, term_to_binary("来自: " ++ integer_to_list(Id) ++ "的群发消息: " ++ MessageInfos)),
  group_send_msg(Infos, Id, MessageInfos).

%% 在线私聊
private_chat(SendId, Socket, MessageInfos) ->
  case ets:match_object(id, {'_', '_', 1, Socket}) of
    [{Id, _, _, _}] ->
      Res = ets:match_object(id, {SendId, '_', 1, '_'}),
      case Res =:= [] of
        true ->
          io:format("~ts~n",[list_to_binary("私聊对象不在线")]);
        _ ->
          private_send_msg(Res, Id, MessageInfos)
      end;
    _ ->
      io:format("~ts~n",[list_to_binary("未登录，私聊失败")])
  end.

%% 私聊发送
private_send_msg([Info], Id, MessageInfos) ->
  {_, _, _, Socket} = Info,
  gen_tcp:send(Socket, term_to_binary("来自: " ++ integer_to_list(Id) ++ "说: " ++ MessageInfos)).