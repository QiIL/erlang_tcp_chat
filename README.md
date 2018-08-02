(1)
说明:
1. C/S架构, 基于TCP socket通信(gen_tcp)
2. 服务端采用固定port来提供服务
3. 客户端登录时需指定所连接的服务器IP, 并给出自己的昵称, 登录不需要密码
4. 一个客户端发言时, 其他所有客户端看到 "[昵称]发言内容"
5. 有人上线时, 已连接的其他客户端需要看到 "[昵称]上线了" 的提示
6. 有人下线时, 已连接的其他客户端需要看到 "[昵称]下线了" 的提示
7. 对于不同的客户端, 昵称不能重复

(2)
1. 添加账号密码验证, 客户端需要使用合适的账号密码才能登录, 服务端需要知道每个账号正确的密码, 用ETS保存
2. 客户端登录后可以自行修改自己的密码
3. 服务器添加手动踢掉(下线)某个账号的功能
4. 同一个账号多客户端登录时, 后登录的把前面的的挤下线
5. 限制每个客户端发言的频率, 任意一分钟内不可以超过50条
6. 添加私聊功能, 私聊时仅对方看到 "[悄悄话][昵称]内容" 形式的消息

(3)
1. 服务端添加在线人数查询
2. 添加自定义讨论组(频道)功能, 讨论组有标题但可以不唯一, 讨论组需要有唯一的ID, 由服务端定义至少2个不同的讨论组
3. 客户端可以选择进入或离开多个讨论组, 可以选择在某个组中发言, 并看到 "[讨论组名][昵称]内容" 形式的对话消息
4. 加入的讨论组, 在下次上线时依然保持在其中
5. 为套接字设计keepsalive机制(如果还没有), 在网络断开时能快速发现
6. 讨论组不用进程管理用数据库管理

(4)
1. 用OTP框架实现系统, 把聊天工具包装为application:
    1) 至少实现1个监控树
    2) 工作进程用gen_server实现

(5)
利用mnesia, 实现: 
1. 服务端重启后与重启前的各功能不变(频道, 密码, etc)
2. 客户端上线后可以看到最近(时间或者条数, 自行定义)聊天的记录
3. 确保各客户端看到的消息顺序一致

(6)
1. 用分层和封装思想优化代码结构
![](http://oqzgtjqen.bkt.clouddn.com/tcp_chat_server_and_client.jpg)
2. 对现有的代码做dialyzer静态分析

(7)
1. 在linux vm上独立部署现有的应用
2. 对现有的应用做压力测试
3. 压力测试考察的度量包含但不限于: 客户端登录频率, 世界频道的吞吐量
```erlang
登陆频率
an erlang shell
> tcp_server:start_link().
another erlang shell
> login_num_test:start(3000).
```
```erlang
世界频道吞吐量（最好注释掉client中handle_info({tcp, Socket, Bin}, Client)函数中的decode(Bin)函数让client端不输出任何内容）。
an erlang shell
> tcp_server:start_link().
> tcp_server:clean_chat_rec().
> Mid = tcp_server:get_msg_server().
> Cid = tcp_server:get_chat_mg_pid().
> process_info(Mid).
> process_info(Cid).
another erlang shell,
> talk_test:start(100, 100). % 100 people in world they send ten message per second.
```
