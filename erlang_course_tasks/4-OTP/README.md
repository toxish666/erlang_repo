### Упражнения


#### Additionals
- [lockman.erl](https://github.com/toxish666/erlang_repo/blob/master/erlang_course_tasks/4-OTP/additional_task/lockman.erl)
Process that grabbed acquire-lock and then accidently died releases it's lock and give it up for another enqueued process. We link every process with lock manager to spectate there for unexpected errors and when we release that lock we unlink that process from manager. 