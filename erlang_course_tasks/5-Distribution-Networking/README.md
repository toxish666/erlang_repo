### Упражнения

### Распределенный Erlang. Задания

- [distrib](https://github.com/toxish666/erlang_repo/tree/master/erlang_course_tasks/5-Distribution-Networking/core/distrib)

Nodes may be spawned with the help of the next command: 'rebar3 shell --sname fiva@localhost --setcookie 'XXXXX'' where fiva@localhost can be any name (local I guess).

There are 3 applications written for the purpouse of the given task: resource discovery application, statistics collector, distributed nodes and workers.

Every node starts distrib application. Nodes randomly choose another node in cluster so that when it's workers fail another node can restore stacked worker's tasks and distribute them among it's own workers.

Resource discovery pattern is used to connect nodes in cluster. There are several node names: 'contactnode@localhost', 'contactnode1@localhost', 'contactnode2@localhost' that can be spawned for other nodes running distrib application. 'contactnode@localhost' is spawned with rebar3 pre-hook, but it will hang there if you exit application without application:stop command or won't kill it manually (with the help of bash script in root directory).

Tasks are distributed between workers in the round-robin manner without any pre-evaluations of their cost.

Otp gen_event behvaiour is used to implement statistics collector. Statistics can be obtained locally or globally across the entire cluster.

### Сетевой Erlang. Задания

- [atm_net](https://github.com/toxish666/erlang_repo/tree/master/erlang_course_tasks/5-Distribution-Networking/core/atm_net)

Application can be run in terminal with 'rebar3 shell'. Clients can be run on localhost with 'telnet localhost 6543'.

<p align="center">
  <img alt="screen1" src="https://github.com/toxish666/erlang_repo/blob/master/erlang_course_tasks/5-Distribution-Networking/core/atm_net/screen1.png" width="400">
</p>

atm_serv here is a worker of atm_net_sup, it accepts new connections, spawn-monitor new processes to which it delegates control over accepted socket. Moreover, it delegates cards (whether it busy or not) and updates storage.

Tcp clients may obtain(insert) only 1 card considering it hasn't already been taken by another tcp client. 

Only 4 clients (or whatever number in config is) can work with atm, other clients receive message "Server capacity is full" and got dropped conection.

Commands that atm terminal can decode (commands can be send through telnet):
- insert card $card_num$
- push button enter
- push button cancel
- push button $num$
- withdraw
- deposit
- stop 


### Additionals

- [lockman_net.erl](https://github.com/toxish666/erlang_repo/tree/master/erlang_course_tasks/5-Distribution-Networking/additional/lockman_net)

Lockman version in network wrapper.

Only those process (tcp client) who grabbed resource can release it (or release it with it's death).

Commands that lockman server can decode (commands can be send through telnet):
- acquire:$key$
- wait:$key$
- release:$key$
- fire:$key$
