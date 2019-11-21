### Упражнения

### Core (Упражнения)

#### 4.1. Db on gen_server
- [mdb_gen.erl](https://github.com/toxish666/erlang_repo/blob/master/erlang_course_tasks/4-OTP/core_tasks/mdb_gen/mdb_gen.erl)

#### 4.2. Банкомат (gen_statem)
- [atm_gen.erl](https://github.com/toxish666/erlang_repo/blob/master/erlang_course_tasks/4-OTP/core_tasks/atm_gen/atm_gen.erl)

#### 4.3 Open Telecom Bank (application + supervisor)
- [bank_otp](https://github.com/toxish666/erlang_repo/tree/master/erlang_course_tasks/4-OTP/core_tasks/bank_otp)

Application can be run with following sequence of commands:
	erl -make
	erl -pa ebin/
	make:all([load]).
	application:start(bank_otp).

To start new atm: bank_otp:start_atm(fiva).

<p align="center">
  <img alt="screen1" src="https://github.com/toxish666/erlang_repo/tree/master/erlang_course_tasks/4-OTP/screen1.png" width="400">
</p>

Here we've got main supervisor which has it's 2 children: main server (for storage and whatnot) and supervisor for future workers. Workers created dynamically. 

Available commands:
- bank_otp:stop_atm(fiva)
- bank_otp:insert_card(fiva, 1234)
- bank_otp:push_button(fiva, enter/cancel/withdraw/deposit/$numbers$)

### Additionals

- [lockman.erl](https://github.com/toxish666/erlang_repo/blob/master/erlang_course_tasks/4-OTP/additional_task/lockman.erl)

Process that grabbed acquire-lock and then accidently died releases it's lock and give it up for another enqueued process. We link every process with lock manager to spectate there for unexpected errors and when we release that lock we unlink that process from manager. 

- [work_dispatcher_gen_server.erl](https://github.com/toxish666/erlang_repo/blob/master/erlang_course_tasks/4-OTP/additional_task/work_dispatcher_gen_server.erl)

There is 2 modes in which this gen_server (read pool) can work: client can add work to dispatcher and wait for it to return synchronously; client can acquire worker from dispatcher and use it sending messages {self(), make_ref(), Work} to complete any work in lambda, but then it needs to release worker so dispatcher grab it to it's pool. Still, when worker dies, it simply gets restarted, so client can wait forever if worker failed during work processing. Supervisor changes to be made (i guess it would be possible to return worker state when it fails).