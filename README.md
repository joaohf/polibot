polibot
=====

A special code and common_test suite to show the ideas from: [Polibot](https://beam-mignon.netlify.com/posts/mignon_8/).

Build
-----

    $ rebar3 compile

Test
-----

    $ make ssh-keygen
    $ make shell
    $ rebar3 ct --spec test/gbs.spec