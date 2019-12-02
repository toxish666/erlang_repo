### ETS key-value distributed storage

Task: "Сделать простое распределённое key-value хранилище на ets. Если erlang-узел запущен в одиночном режиме, то он выдает данные со своего локального ets, если же в распределённом режиме и на других узлах также запущен процесс с ets, то запрашивать данные и с удалённых узлов; при этом должны поддерживаться простые функции select".

Nodes can be spawned with 'rebar3 shell --sname anyname --setcookie 'XXXXX'' command. It is further necessary to connect any nodes with net_adm:ping('anyname') or similar. 

When there's no node beside only one, both '_local' and '_replicated' functions will get the output but in different format.

get_with_fun functions will get any matching spec for ets as argument. It is better to pass it specification compiled with ets:fun2ms(ActualFunction).