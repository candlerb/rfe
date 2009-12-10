CEAN
====

The Comprehensive Erlang Archive Network is the nearest erlang has to
rubygems.

Getting started is harder than it should be. For a start, ubuntu doesn't
have a cean package, and furthermore it in turn depends on ibrowse and
there's no package for that either.

Download the cean client tarball and install it into your erlang lib
directory - or install it into any directory you like, as long as the
ERL_LIBS environment variable points at that location.

    wget http://cean.process-one.net/downloads/cean.tar.gz
    sudo tar -C /usr/lib/erlang/lib -xvzf cean.tar.gz

You'll also need to install ibrowse, which you can download from the cean
site, but once untarred you need to compile it:

    cd src
    mkdir ../ebin
    make

Using CEAN
----------

    cean:help().
    cean:search(ldap).
    [{"erldir","erldir is an open source DNS and LDAP server"},
     {"eldap","This is the Erlang LDAP library"}]

    cean:install(eldap).
    rr(eldap).

*** BUT it does not work for me: error I get always is

    failed to find dependencies: no_pub_file

Maybe my ibrowse install is not right? Or perhaps there are no R13B packages
yet? cean:available() does a
GET /R13B/devel/linux-x86.vsnlst HTTP/1.1
