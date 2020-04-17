## 描述
   一个可自定义的tcp管子(如可以配置http_proxy,port_forwarding)



## 启动
    1.编译make
    2.修改配置文件config/tcp_pipe.config(参考例子config/tcp_pipe.config.example)
    3.运行erl -pa ebin -config config/tcp_pipe -s tcp_pipe -detached
    (参考运行或参考Makefile中的标签, 如:
        tcp_pipe_tests
        dist_tcp_pipe_tests
        dist_aa_cc_tcp_pipe_tests
        http_proxy_tests
        http_proxy_aa_cc_dist_tests
    其中有 注释, 启动命令 并可以参照其中的配置文件)

## 停止
    TODO 停止应用脚本没时间写，先用这个吧
    1.  在shell中查找相关进程"ps -ef|grep beam|grep tcp_pipe"
    2.  杀死查找到的进程"kill -9 进程id"

## 扩展
    1.添加子管子模块文件如src/sub_pips/http_proxy.erl
    2.实现回调函数
        % 发送前处理数据
        handle(send, {Data0, State0}) -> 
        % 接受后处理数据
        handle(recive, {Data0, State0}) -> {ok, {Data1, State1}} | {error, Module, Reason}
    3.把子管子模块添加到配置文件, 如config/http_proxy.config
        ... 
        {sub_pipes,[http_proxy]}
        ... 

## 常见问题和报错

    问题：local app(如浏览器)一直加载, 无响应
        原因1: 由于没联网, 导致服务连接DNS服务器, 导致域名无法解析
        原因2: 内网DNS一般无法解析外网域名或只能解析一部分(这和中国这个大内网DNS受污染无法解析google域名一样)
        解决1: 把域名对应ip加到系统的hosts文件中
        解决2: 直接用ip访问
               (先去可以访问域名的主机"ping 域名"就可以得到ip了)

    报错: 包含"error,eaddrinuse"
        原因1: 关闭后tcp端口正在处于等待状态
        解决1: 一般等1-10分钟再启动(尤其是运行测试后), 
        原因2: ip或端口正在被占用
        解决2: 换一个端口 或杀死占用端口的进程

    报错:{{badmatch,{error,http_proxy,{badmatch,{error,nxdomain}}}},
        原因: 由于没联网, 导致服务连接DNS服务器, 导致域名无法解析
        解决: 忽略

    报错:{{badmatch,{error,einval}}, [{tp_side_mm,handle_cast,2},
        原因: 未知
        解决: 忽略


