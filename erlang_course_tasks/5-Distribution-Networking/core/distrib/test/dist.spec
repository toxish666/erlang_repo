{node, a, 'fiva1@Antons-MacBook-Pro.local'}.
{node, b, 'fiva2@Antons-MacBook-Pro.local'}.
{node, c, 'fiva3@Antons-MacBook-Pro.local'}.

{init, [a,b,c], [{node_start, [
		      {monitor_master, true},
		      {erl_flags, "--setcookie 'XXXXX'"}
		  ]}]}.

{alias, dist, "./dist/"}.

{logdir, all_nodes, "./logs/"}.
{logdir, master, "./logs/"}.

{suites, [a,b,c], dist, all}.
{skip_cases, [a], dist, distrib_SUITE, test, "This test fails on purpose"}.
