-record(ircmsg, {prefix = <<>>, 
                 command = <<>>, 
                 arguments = [], 
                 tail = <<>>}).

-record(serverconfig, {name="unnamed",
                       hostname,
                       port=6667,
                       nick="dingd1ding",
                       channels=[]}).
