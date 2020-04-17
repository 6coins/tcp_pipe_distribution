
-record(state, {module, % 回调模块
                side,                                                           % 此端名
                type,                                                           % 此端的类型
                listen,                                                         % 监听
                socket,                                                         % 连接
                other_child,                                                    % 另一端对应的子进程的pid
                side_sup_name,                                                  % 此端监督进程
                other_side_sup_name,                                            % 另一端端监督进程
                config,                                                         % 此端配置
                socket_dist                                                     % 用于socket分布的模块
               }).

