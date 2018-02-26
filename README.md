# RabbitMQ Message Persistor Plugin

This plugin persists messages by filling the `delivery_mode` property of a
message as it enters RabbitMQ with the protocol defined value of `2`, regardless
of the what value was set/defined by the publishing client application. If
enabled, this allows all RabbitMQ ingress messages to be marked as
persistent on publish entry.

## Supported RabbitMQ Versions

This plugin targets RabbitMQ 3.6.0 and later versions.

## Limitations

This plugin cannot be used together with the following plugins:
- [rabbitmq-message-timestamp](https://github.com/rabbitmq/rabbitmq-message-timestamp)
- [rabbitmq-routing-node-stamp](https://github.com/rabbitmq/rabbitmq-routing-node-stamp)
as they override the same extension point.

## Installation

See [Plugin Installation](http://www.rabbitmq.com/installing-plugins.html) for details
about how to install plugins that do not ship with RabbitMQ.

## Building from Source

You can build and install it like any other plugin (see
[the plugin development guide](http://www.rabbitmq.com/plugin-development.html)).

## Usage ##

Just enable the plugin with the following command:

```bash
rabbitmq-plugins enable rabbitmq_message_persistor
```

The plugin will then hook into the `basic.publish` process in order to
set messages as persistent from the broker's perspective.

## Limitations ##

The plugin hooks into the `basic.publish` path, so expect a small
throughput reduction when using this plugin, since it has to modify
every message that crosses RabbitMQ.

This plugin should not be enabled at the same time as any other
interceptors  that hook into the `basic.publish` process, such as
the  `rabbitmq-routing-node-stamp` plugin. Enabling more than one
interceptor that is registered to the `basic.publish` process will
cause all AMQP 0-9-1 connections to fail when creating a new channel.

## LICENSE ##

See the LICENSE file
