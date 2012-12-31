MeeQo
=====

MeeQo is an open source message queue. I got most ideas about it from **ZeroMQ** and take the advantages of **Erlang** , which is currently my favourite programming language, to implement a more easy-to-use message queue. MeeQo is just an encapsulation of BSD socket with no supports of subject and filter. There are only two protocols, **read-send** and **tweet**. You CANNOT mix them in one TCP connection and there is no need to do so. **I HATE tedious user mannual**, so just read the following examples, which will help you understand **ALL** the usages.

*****
Start two MeeQo instances on port 6611 and 6613 respectively. The port number MUST be **odd**.

<pre><code>/path/to/meeqo/ebin$ erlc ../src/*erl
/path/to/meeqo/ebin$ erl
Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]<br />
Eshell V5.9.1  (abort with ^G)
1> meeqo:start_link(6611).
{ok,`<0.43.0>`}
2> meeqo:start_link(6613).
{ok,`<0.51.0>`}
3></pre></code>

The format of read-send protocol is as follows. Any character can be inserted between first two "[" and last two "]" only if the message body can be distinguished, e.g. `[@#[Hello]]MeeQo]@#]`, of which the message body is `Hello]]MeeQo`. This is a trick of **Lua** syntax. What is more, message can be sent in multiple continuous TCP packets. (Refer the second message.) After all the data in a single message is sent to MeeQo, a reply "ok" will be return.

<pre><code>read                            - Read the first message and delete it from MeeQo.
read IP:port                    - Read the first message from IP:port and delete it from MeeQo.
send IP:port [...[message]...]  - Send a message to MeeQo at IP:port.
</code></pre>

<pre><code>~$ ncat 192.168.3.139 6611
send 192.168.3.139:6613 [`[The First Message]`]
ok
send 192.168.3.139:6613 [`[The First Line of The Second Message`<br />`
The Second Line of the Second Message]`]
ok
^C</pre></code>

<pre><code>~$ ncat 192.168.3.139 6613
read
The First Message
read 192.168.3.139:6611
The First Line of The Second Message
The Second Line of the Second Message
send 192.168.3.139:6611 [`[I got two messages from you]`]
ok
^C</pre></code>

<pre><code>~$ ncat 192.168.3.139 6611
read
I got two messages from you
^C</pre></code>

Different from the former protocol, we can tweet multiple messages in one TCP packet, or more exactly, in one TCP connection. But each message MUST end with a **null character**('\0'), which means the message body cannot contain it. You can input it in CLI by pressing `<C-2>`.

<pre><code>~$ ncat 192.168.3.139 6611
tweet 192.168.3.139:6613 Message One^@
tweet 192.168.3.139:6613 Message Two^@tweet 192.168.3.139:6613 Message Three^@
tweet 192.168.3.139:6613 "^@" is null character.^@
^C</pre></code>

<pre><code>~$ ncat 192.168.3.139 6613
read
Message One
read 192.168.3.139:6611
Message Two
read
Message Three
read
"^@" is null character.
^C</pre></code>

***
This is how to use MeeQo via **Ncat**. You can use most of the programming languages to implement such a client.
