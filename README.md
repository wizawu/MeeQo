MeeQo
=====

MeeQo is an open source message queue. I got most ideas about it from **ZeroMQ** and take the advantages of **Erlang** , which is currently my favourite programming language, to implement a more easy-to-use message queue. MeeQo is just an encapsulation of BSD socket with no supports of subject and filter. There are only two protocols, **read-send** and **tweet**. You CANNOT mix them in one TCP connection and there is no need to do so. **I HATE tedious user mannual**, so just read the following examples, which will help you understand **ALL** the usages.

*****
Start two MeeQo instances on port 6611 and 6613 respectively. The port number MUST be **odd**.

<pre><code>/path/to/meeqo$ mkdir ebin && make
/path/to/meeqo$ erl -pa ebin
Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]<br />
Eshell V5.9.1  (abort with ^G)
1> meeqo:start_link(6611).
{ok,&lt;0.43.0&gt;}
2> meeqo:start_link(6613).
{ok,&lt;0.51.0&gt;}
3></code></pre>

The format of read-send protocol is as follows. Any character can be inserted between first two "[" and last two "]" only if the message body can be distinguished, e.g. <code>[@#[Hello]]MeeQo]@#]</code>, of which the message body is <code>Hello]]MeeQo</code>. This is a trick of <strong>Lua</strong> syntax. What is more, message can be sent in multiple continuous TCP packets. (Refer the second message.) After all the data in a single message is sent to MeeQo, a reply "ok" will be return.

<pre><code>read                            - Read the first message and delete it from MeeQo.
read IP:port                    - Read the first message from IP:port and delete it from MeeQo.
send IP:port [...[message]...]  - Send a message to MeeQo at IP:port.

PLEASE DO NOT ENTER REDUNDANT WHITESPACES.
</code></pre>

<pre><code>~$ ncat 192.168.3.139 6611
send 192.168.3.139:6613 [[The First Message]]
<i>ok</i>
send 192.168.3.139:6613 [[The First Line of The Second Message
The Second Line of the Second Message]]
<i>ok</i>
^C</code></pre>

<pre><code>~$ ncat 192.168.3.139 6613
read
<i>The First Message</i>
read 192.168.3.139:6611
<i>The First Line of The Second Message
The Second Line of the Second Message</i>
send 192.168.3.139:6611 [[I got two messages from you]]
<i>ok</i>
^C</code></pre>

<pre><code>~$ ncat 192.168.3.139 6611
read
<i>I got two messages from you</i>
^C</code></pre>

Different from the former protocol, we can tweet multiple messages in one TCP packet, or more exactly, in one TCP connection. But each message MUST end with a **null character**('\0'), which means the message body cannot contain it. You can input it in CLI by pressing `<C-2>`.

<pre><code>~$ ncat 192.168.3.139 6611
tweet 192.168.3.139:6613 Message One^@
tweet 192.168.3.139:6613 Message Two^@tweet 192.168.3.139:6613 Message Three^@
tweet 192.168.3.139:6613 "^@" is null character.^@
^C</code></pre>

<pre><code>~$ ncat 192.168.3.139 6613
read
<i>Message One</i>
read 192.168.3.139:6611
<i>Message Two</i>
read
<i>Message Three</i>
read
<i>"^@" is null character.</i>
^C</code></pre>

This is how to use MeeQo via **Ncat**. You can implement such a client with a
few lines of code in Java or Python, etc.

*****
### Benchmark  
#### Localhost
<pre><code>    100,000   | 8 bytes | 64 bytes
--------------|---------|----------
   Beanstalk  |  22.32s |  18.89s
--------------|---------|----------
 MeeQo(tweet) |  13.31s |  17.49s
--------------|---------|----------
  MeeQo(send) |  20.37s |  22.35s</code></pre>
#### 1Gbit/s Ethernet
<pre><code>    100,000   | 8 bytes | 64 bytes
--------------|---------|----------
   Beanstalk  | 123.47s | 122.69s
--------------|---------|----------
 MeeQo(tweet) |  98.74s |  93.18s
--------------|---------|----------
  MeeQo(send) | 203.02s | 211.17s</code></pre>
