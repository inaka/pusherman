1. Pusherman

1. Pusherman is an Erlang service that accepts and delivers push notifications, currently for Apple. It uses apns4erl but it provides a UDP and HTTP interface for pushes, and queues them. It has a pluggable queue backend so you can queue pushes in whatever system you want. Currently it uses leveldb but will eventually support Redis and Rabbit.

The idea with pusherman is you want to abstract away the calls to apple and queue up your pushes. Say you have a lot of pushes and want to manage connections to apple, etc. The goal is for this to be the place to do that. We'll add some graphical UI and statsd support so you can track how things are going.


Todo
====

* TESTS
* UDP
* priority queues
* stats
* feedback channel needs to POST back to the caller
* get_parameter is floating out in the push_handler_api code
