# NOTICE
**This project is not supported anymore.**
It's still here just to support legacy projects that may have it as a dependency.
It's operational (at least, for Erlang versions lower than R17)

# Pusherman

Pusherman is an Erlang service that accepts and delivers push notifications, currently for Apple. It uses apns4erl but it provides a UDP and HTTP interface for pushes, and queues them. It has a pluggable queue backend so you can queue pushes in whatever system you want. Currently it uses leveldb but will eventually support Redis and Rabbit.

The idea with pusherman is you want to abstract away the calls to apple and queue up your pushes. Say you have a lot of pushes and want to manage connections to apple, etc. The goal is for this to be the place to do that. We'll add some graphical UI and statsd support so you can track how things are going.

Contact Us
==========

For **questions** or **general comments** regarding the use of this library, please use our public
[hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please [open an issue](https://github.com/inaka/pusherman/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)


Todo
====

* TESTS
* UDP
* priority queues
* stats
* feedback channel needs to POST back to the caller
* get_parameter is floating out in the push_handler_api code
