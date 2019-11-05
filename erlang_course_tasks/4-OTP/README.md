### Упражнения


#### Additionals
- [lockman.erl](https://github.com/toxish666/erlang_repo/blob/master/erlang_course_tasks/4-OTP/additional_task/lockman.erl)
Process that grabbed acquire-lock and then accidently died releases it's lock and give it up for another enqueued process. We link every process with lock manager to spectate there for unexpected errors and when we release that lock we unlink that process from manager. 

- [work_dispatcher_gen_server.erl](https://github.com/toxish666/erlang_repo/blob/master/erlang_course_tasks/4-OTP/additional_task/work_dispatcher_gen_server.erl)
There is 2 modes in which this gen_server (read pool) can work: client can add work to dispatcher and wait for it to return synchronously; client can acquire worker from dispatcher and use it sending messages {self(), make_ref(), Work} to complete any work in lambda, but then it needs to release worker so dispatcher grab it to it's pool. Still, when worker dies, it simply gets restarted, so client can wait forever if worker failed during work processing. Supervisor changes to be made (i guess it would be possible to return worker state when it fails).