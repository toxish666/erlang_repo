-define(PUBLICKEYBYTES, libsodium_crypto_box_curve25519xsalsa20poly1305:publickeybytes()).
-define(SECRETKEYBYTES, libsodium_crypto_box_curve25519xsalsa20poly1305:secretkeybytes()).
-define(NONCEBYTES, libsodium_crypto_box_curve25519xsalsa20poly1305:noncebytes()).

-define(KBUCKET_DEFAULT_SIZE, 8).

-define(KBUCKET_MAX_ENTRIES, 255).

-define(TAGBYTES, 16).

-define(REQUESTBYTES, 8).
