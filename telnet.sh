#!/bin/sh
console()
{
    echo "test"
    erl -boot start_sasl -config app.config -pa ebin deps/*/ebin -s webserver
}

$@