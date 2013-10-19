# Overview
This is a web Instant Messaging plugin. This is the rabbitmq-plugin
port of the original webim, which is implemented based on node.js.

This version of webim improves in several positions:
1. multiple users are supported.
2. long-polling is used, it's much better than the php port of webim.
3. the users and their relations are stored in mnesia database,
   each user has a corresponding persistent message queue in rabbitmq.
   no third-party persistent utility, such as mysql and redis, is used.
   the whole webim is running in the same erlang virtual machine.
   so the total memory usage is lower.
4. the same mochiweb wrapper plugin is used, just as rabbitmq_management
   plugin.
5. No two session for the same user is allowed. The latter one will
   terminate the former one.
6. It's easy to use multiple nodes to run webim, based on erlang and
   rabbitmq's distributed architecture.


At first, cowboy web server is considered, and there's one cowboy wrapper
plugin already. But the default cowboy version is too low. so at last
mochiweb wrapper plugin is used.

Each active user has one corresponding process, which is used to consume
messages from the user's message queue. No prefetch count is used, so
the message will arrive this process as soon as it is available in the 
message queue, even the former messages are not acknowledged.

One user's messages are persistently stored in his/her own message queue.
It will be consumed by the active session of the user. But the messages
will only be removed from the mq when the user click 'clean' button to
remove the received message. Then the messages received by the user will
be acknowledged and the messages will be removed.

The process corresponding to the active user maintain one queue to store
the received messages from the mq. these messages will be long-polled by
the users via a browser.

# Installation
* make sure erlang/OTP is installed correctly
* download rabbitmq from http://www.rabbitmq.com, unzip/install it.
* download the current packaged version of webim from download directory,
  put it into rabbitmq's plugins directory
* run command as follows to enable the webim plugin:
    rabbitmq-plugins enable rabbitmq-webim
* start rabbitmq:
    rabbitmq-server
* now access http://localhost:8000 to access webim.

# Compilation
reference rabbitmq plugin development guide:
    http://www.rabbitmq.com/plugin-development.html
put rabbitmq-webim input the rabbitmq-public-umbrella directory, then hacking.
