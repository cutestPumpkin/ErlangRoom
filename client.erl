%%%-------------------------------------------------------------------
%%% @author Pumpkin
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 8月 2021 14:03
%%%-------------------------------------------------------------------
-module(client).
-author("Pumpkin").
-define(DEFAULT_PORT, 1055).
%% API
-compile(export_all).

%客户端
s() -> start_client(?DEFAULT_PORT).

start_client(Port) ->
  {ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}]),  %连接服务器
  %新建一个进程负责接收消息
  Pid = spawn(fun() -> loop() end),
  gen_tcp:controlling_process(Socket, Pid),
  sendMsg(Socket).

loop() ->
  receive
    {tcp, _Socket, Bin} ->
      io:format("~ts ~ts ~n", [list_to_binary("收到消息："), list_to_binary(binary_to_term(Bin))]),
      loop();
    {tcp_closed, _Socket} ->
      io:format("~ts ~n", [list_to_binary("连接关闭！")])
  end.

sendMsg(Socket) ->
  S = io:get_line(list_to_binary("选择你的操作(1注册，2登录，3注销，4私聊，5群聊):")),
  {Sign, _Info} = string:to_integer(S),
  SendMsg = operation_message(Sign),
  gen_tcp:send(Socket, term_to_binary(SendMsg)),
  sendMsg(Socket).

%% 用户注册
operation_message(1) ->
  I = io:get_line(list_to_binary("输入注册用户id（整数）：")),
  {Id, _Info} = string:to_integer(I),
  Password = io:get_line(list_to_binary("输入注册密码：")),
  [Id, register_user, Password, 0, 0];
%% 用户登录
operation_message(2) ->
  I = io:get_line(list_to_binary("输入你的用户id（整数）：")),
  Password = io:get_line(list_to_binary("输入你的登录密码：")),
  {Id, _Info} = string:to_integer(I),
  [Id, login_user, Password, 0, 0];
%% 用户退出
operation_message(3) ->
  I = io:get_line(list_to_binary("输入你的用户名id（整数）：")),
  {Id, _Info} = string:to_integer(I),
  [Id, login_out, 0, 0, 0];
%% 私聊
operation_message(4) ->
  Sd = io:get_line(list_to_binary("输入发送信息的他人用户id（整数）：")),
  Msg = io:get_line(list_to_binary("发送信息：")),
  {SendId, _Info} = string:to_integer(Sd),
  [0, private_msg, 0, SendId, Msg];
%% 群聊
operation_message(5) ->
  Msg = io:get_line(list_to_binary("发送信息：")),
  [0, group_msg, 0, 0, Msg];
%% 无效操作
operation_message(_) ->
  Msg = io:format(list_to_binary("无效操作，请重新输入：")),
  [0, invalid_operation, 0, 0, Msg].