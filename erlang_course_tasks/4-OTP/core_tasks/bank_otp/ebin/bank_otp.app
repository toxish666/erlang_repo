{
 application, 
 bank_otp,
 [
  {vsn, "1.0.0"},
  {applications,
   [
    kernel,
    stdlib
   ]
  },
  {env, [
      {state_timeout, 10000},
      {max_attempts, 3},
      {storage_file_name, "priv/storage"}
    ]},
	 
  {modules, []},
  {registered, [bank_otp]},
  {mod, {bank_otp, []}}
 ]
}.
