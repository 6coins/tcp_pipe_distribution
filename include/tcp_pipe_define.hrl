%-define(INFO, error_logger:error_msg).

%% socket分布步骤日志
-define(SD_LOG, io:format).
%-define(SD_LOG, pipe_util:no_print).

%% 一端接受到连接后的步骤日志
-define(ACCEPT_INFO, io:format).
%-define(ACCEPT_INFO, pipe_util:no_print).
