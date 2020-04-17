
# 默认
all: compile

# 编译
compile:
	@cp -f src/*/*.app.src ebin/  
#@cd ebin/ && rename app.src app *.app.src
	@mv ebin/tcp_pipe.app.src ebin/tcp_pipe.app
	@erl -make -smp disable 

# 清理
clean:
	@rm -rf ebin/*
	@rm -rf tags
	@rm -rf TAGS
	@rm -rf erl_crash.dump

# 工具模块测试
pipe_util_tests: compile
	@erl -pa ebin +K true -s eunit test pipe_util -s init stop

# 应用的简单测试
tcp_pipe_tests: compile
	@echo " 测试需要执行步骤如下"
	@echo " 在终端1, 启动应用"
	@echo " 	erl -sname tpsd_one -setcookie x -pa ebin +K true -config config/tcp_pipe_tests -s tcp_pipe"
	@echo " "
	@echo " 在终端2, 启动服务2(接收应用的2端的连接, 收a发b)"
	@echo " 	erl -pa ebin -s tcp_pipe_tests server_two"
	@echo " "
	@echo " 在终端3, 启动服务1(连接应用的1端, 发a收b)"
	@echo " 	erl -pa ebin -s tcp_pipe_tests server_one"
	@echo " "
	@echo " 如果在终端4输出 recive b 证明测试通过, 否则终端中会有报错信息"
	@echo " "
	@echo " 测试过程叙述如下"
	@echo " 服务1"
	@echo " --a--> "
	@echo " 应用的tp_side1进程(handle_info/2) "
	@echo " --{send, a}--> "
	@echo " 应用2的tp_side2进程(handle_cast/3) "
	@echo " --a-->"
	@echo " 服务2 "
	@echo " --b-->"
	@echo " 应用2的tp_side2进程(handle_info/2) "
	@echo " --{send, b}--> "
	@echo " 应用1的tp_side1进程(handle_cast/3) "
	@echo " --b-->"
	@echo " 服务1" 
	
# socket分布应用的简单测试
dist_tcp_pipe_tests: compile
	@echo " 测试需要执行步骤如下"
	@echo " 在终端1, 启动应用1"
	@echo " 	erl -sname tpsd_one -setcookie x -pa ebin +K true -config config/dist_tcp_pipe_tests1 -s tcp_pipe"
	@echo " "
	@echo " 在终端2, 启动应用2"
	@echo " 	erl -sname tpsd_two -setcookie x -pa ebin +K true -config config/dist_tcp_pipe_tests2 -s tcp_pipe"
	@echo " "
	@echo " 在终端3, 启动服务2(接收应用2的2端的连接, 收a发b)"
	@echo " 	erl -pa ebin -s tcp_pipe_tests server_two"
	@echo " "
	@echo " 在终端4, 启动服务1(连接应用1的1端, 发a收b)"
	@echo " 	erl -pa ebin -s tcp_pipe_tests server_one"
	@echo " "
	@echo " 如果在终端4输出 recive b 证明测试通过, 否则终端中会有报错信息"
	@echo " "
	@echo " "
	@echo " 测试过程叙述如下"
	@echo " 服务1"
	@echo " --a--> "
	@echo " 应用1的tp_side1进程(handle_info/2) "
	@echo " --{send, a}--> "
	@echo " 应用1的tp_side_mm2进程(handle_cast/3) "
	@echo " --{request_mm, {gen_server, call, [{send, a}]}} -->"
	@echo " 应用2的tp_side_mm1进程(handle_info/2) "
	@echo " --{send, a}--> "
	@echo " 应用2的tp_side2进程(handle_cast/3) "
	@echo " --a-->"
	@echo " 服务2 "
	@echo " --b-->"
	@echo " 应用2的tp_side2进程(handle_info/2) "
	@echo " --{send, b}--> "
	@echo " 应用2的tp_side_mm1进程(handle_cast/3) "
	@echo " --{request_mm, {gen_server, call, [{send, b}]}} -->"
	@echo " 应用1的tp_side_mm2进程(handle_info/2) "
	@echo " --{send, b}--> "
	@echo " 应用1的tp_side1进程(handle_cast/3) "
	@echo " --b-->"
	@echo " 服务1" 
	@echo " "
	
# socket分布应用的特殊情况测试(即非中间人端配置和分布配置中的type都是accept或connect)
dist_aa_cc_tcp_pipe_tests: compile
	@echo " 测试需要执行步骤如下"
	@echo " 在终端1, 启动应用1"
	@echo " 	erl -sname tpsd_one -setcookie x -pa ebin +K true -config config/dist_aa_cc_tcp_pipe_tests1 -s tcp_pipe"
	@echo " "
	@echo " 在终端2, 启动应用2"
	@echo " 	erl -sname tpsd_two -setcookie x -pa ebin +K true -config config/dist_aa_cc_tcp_pipe_tests2 -s tcp_pipe"
	@echo " "
	@echo " 在终端3, 启动服务2(接收应用2的2端的连接, 收a发b)"
	@echo " 	erl -pa ebin -s tcp_pipe_tests server_two"
	@echo " "
	@echo " 在终端4, 启动服务1(连接应用1的1端, 发a收b)"
	@echo " 	erl -pa ebin -s tcp_pipe_tests server_one"
	@echo " "
	@echo " 如果在终端4输出 recive b 证明测试通过, 否则终端会有报错信息"
	@echo " "
	@echo " 测试过程同dist_tcp_pipe_tests"

# http代理测试
http_proxy_tests: compile
	@echo " 测试需要执行步骤如下"
	@echo " 在终端, 启动应用"
	@echo " 	erl -sname tpsd_one -setcookie x -pa ebin +K true -config config/http_proxy -s tcp_pipe"
	@echo " 根据应用启动的ip和配置中的端口7771，在浏览器中设置http代理"
	
# http代理分布测试(两个应用可以断线重连)
http_proxy_dist_tests: compile
	@echo " 测试需要执行步骤如下"
	@echo " 在主机1启动应用(这台机器允许浏览器通过7771连接她)"
	@echo " 	erl -sname tpsd_one -setcookie x -pa ebin +K true -config config/http_proxy_dist1 -s tcp_pipe"
	@echo " 在主机2启动应用(这台机器允许主机1通过7772连接她 且 允许连接目标主机)"
	@echo " 	erl -sname tpsd_two -setcookie x -pa ebin +K true -config config/http_proxy_dist2 -s tcp_pipe"
	@echo " 根据主机1的ip和配置中的端口7771，在浏览器中设置http代理"
	
# http代理特殊分布测试(两个应用不可以处理断线重连)
http_proxy_aa_cc_dist_tests: compile
	@echo " 测试需要执行步骤如下"
	@echo " 在主机1启动应用(这台机器允许浏览器通过7771连接她 且 允许主机2通过7772连接她)"
	@echo " 	erl -sname tpsd_one -setcookie x -pa ebin +K true -config config/http_proxy_aa_cc_dist1 -s tcp_pipe"
	@echo " 在主机2启动应用(这台机器 允许连接目标主机)"
	@echo " 	erl -sname tpsd_two -setcookie x -pa ebin +K true -config config/http_proxy_aa_cc_dist2 -s tcp_pipe"
	@echo " 根据主机1的ip和配置中的端口7771，在浏览器中设置http代理"


