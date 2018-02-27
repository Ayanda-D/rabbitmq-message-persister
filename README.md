# RabbitMQ Message Persister Plugin

This plugin persists messages by filling the `delivery_mode` property of a
message as it enters RabbitMQ with the AMQP 0-9-1 protocol defined setting of
`2`, regardless of the what value was set/defined by the publishing client
application. If enabled, it allows all RabbitMQ ingress messages to be marked as
persistent on publish entry.

**NOTE:** The plugin assumes queues to which messages are destined to were
declared as **durable**, along with **durable exchanges** if **full message**
persistence is the desired outcome.

## Supported RabbitMQ Versions

This plugin targets RabbitMQ 3.6.0 and later versions.

## Limitations

This plugin cannot be used together with the following plugins:
- [rabbitmq-message-timestamp](https://github.com/rabbitmq/rabbitmq-message-timestamp)
- [rabbitmq-routing-node-stamp](https://github.com/rabbitmq/rabbitmq-routing-node-stamp)

as they override the same extension point.

## Installation

To create a package, execute `make dist` and find the `.ez` package file in the
`plugins` directory. Refer to the standard [RabbitMQ Plugin Installation Guide](http://www.rabbitmq.com/installing-plugins.html) for more details on
installing plugins that do not ship with RabbitMQ by default.

## Testing

Clone and execute `make tests` to test the plugin. View test results from the
generated HTML files.

## Building from Source

You can build and install it like any other plugin (see
[the plugin development guide](http://www.rabbitmq.com/plugin-development.html)).

## Usage ##

Just enable the plugin with the following command:

```bash
rabbitmq-plugins enable rabbitmq_message_persister
```

The plugin will then hook into the `basic.publish` process in order to
set messages as persistent from the broker's perspective.

## Limitations ##

The plugin hooks into the `basic.publish` path, so expect a small
throughput reduction when using this plugin, since it has to modify
every message that crosses RabbitMQ.

This plugin should not be enabled at the same time as any other
interceptors  that hook into the `basic.publish` process, such as
the `rabbitmq-message-timestamp` and `rabbitmq-routing-node-stamp` plugins.
Enabling more than one interceptor that is registered to the `basic.publish`
process will cause all AMQP 0-9-1 connections to fail when creating a new channel.

## LICENSE ##

(c) Erlang Solutions Ltd. 2017-2018

https://www.erlang-solutions.com/
