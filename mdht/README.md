mdht
=====

An OTP application

Build
-----

    $ rebar3 compile



If you're a MAC user than application might not run with error message: 'autoconf is required, but wasn't found on this system'. The solution is to install it with 'brew install libtool autoconf automake'.


### Notes.
To debug the system there was added logger:debug(...) commands. In order to see it's output in eunit tests I used the following commands:
- rebar3 as test shell (note that sys.config should be filled accordingly)
- eunit:test(module, [verbose]).