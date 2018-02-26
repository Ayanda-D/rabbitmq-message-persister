%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ Message Persister.
%%
%% The Initial Developer of the Original Code is Erlang Solutions, Inc.
%% Copyright (c) 2017-2018 Erlang Solutions Ltd.  All rights reserved.
%%

-module(rabbit_message_persister_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("rabbit_message_persister.hrl").

-define(SEND_DELAY, 1000).

all() ->
    [
      {group, non_parallel_tests}
    ].

groups() ->
    [
      {non_parallel_tests, [], [
                                message_persisted_test,
                                existing_message_persisted_test
                               ]}
    ].

%% -------------------------------------------------------------------
%% Testsuite setup/teardown.
%% -------------------------------------------------------------------

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    Config1 = rabbit_ct_helpers:set_config(Config, [
        {rmq_nodename_suffix, ?MODULE}
      ]),
    rabbit_ct_helpers:run_setup_steps(Config1,
      rabbit_ct_broker_helpers:setup_steps() ++
      rabbit_ct_client_helpers:setup_steps()).

end_per_suite(Config) ->
    rabbit_ct_helpers:run_teardown_steps(Config,
      rabbit_ct_client_helpers:teardown_steps() ++
      rabbit_ct_broker_helpers:teardown_steps()).

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_started(Config, Testcase).

end_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, Testcase).

%% -------------------------------------------------------------------
%% Testcases.
%% -------------------------------------------------------------------

message_persisted_test(Config) ->
    Chan = rabbit_ct_client_helpers:open_channel(Config, 0),

    Ex = <<"e1">>,
    Q = <<"q">>,

    setup_fabric(Chan, make_exchange(Ex, <<"direct">>), make_queue(Q)),

    Msgs = [1, 2, 3, 4, 5],

    amqp_channel:call(Chan, #'confirm.select'{}),

    publish_messages(Chan, Ex, Msgs),

    amqp_channel:wait_for_confirms_or_die(Chan),

    {ok, Result} = consume(Chan, Q, Msgs, 5000),

    [begin
         ?assertNotEqual(get_message_delivery_mode(Msg), undefined),
         ?assert(is_integer(get_message_delivery_mode(Msg))),
         ?assert(get_message_delivery_mode(Msg) =:= ?PERSIST_MESSAGE_DELIVERY_MODE)
     end|| Msg <- Result],

    amqp_channel:call(Chan, delete_queue(Q)),
    amqp_channel:call(Chan, delete_exchange(Ex)),

    rabbit_ct_client_helpers:close_channel(Chan),
    passed.

existing_message_persisted_test(Config) ->
    Chan = rabbit_ct_client_helpers:open_channel(Config, 0),

    Ex = <<"e1">>,
    Q = <<"q">>,

    setup_fabric(Chan, make_exchange(Ex, <<"direct">>), make_queue(Q)),

    Msgs = [1, 2, 3, 4, 5],

    amqp_channel:call(Chan, #'confirm.select'{}),

    publish_persisted_messages(Chan, Ex, Msgs),

    amqp_channel:wait_for_confirms_or_die(Chan),

    {ok, Result} = consume(Chan, Q, Msgs, 5000),

    [begin
         ?assertNotEqual(get_message_delivery_mode(Msg), undefined),
         ?assert(is_integer(get_message_delivery_mode(Msg))),
         ?assert(get_message_delivery_mode(Msg) =:= ?PERSIST_MESSAGE_DELIVERY_MODE)
     end|| Msg <- Result],

    amqp_channel:call(Chan, delete_queue(Q)),
    amqp_channel:call(Chan, delete_exchange(Ex)),

    rabbit_ct_client_helpers:close_channel(Chan),
    passed.


%% -------------------------------------------------------------------
%% Implementation.
%% -------------------------------------------------------------------

get_payload(#amqp_msg{payload = P}) ->
  binary_to_term(P).

get_message_delivery_mode(#amqp_msg{props = #'P_basic'{delivery_mode = DM}}) ->
    DM.

get_single_header(Target,
    #amqp_msg{props = #'P_basic'{headers = Headers}}) ->
        lists:keyfind(Target, 1, Headers).

setup_fabric(Chan, ExDeclare, QueueDeclare) ->
    setup_fabric(Chan, ExDeclare, QueueDeclare, <<>>).

setup_fabric(Chan,
             ExDeclare = #'exchange.declare'{exchange = Ex},
             QueueDeclare,
             RK) ->
    declare_exchange(Chan, ExDeclare),

    #'queue.declare_ok'{queue = Q} =
        amqp_channel:call(Chan, QueueDeclare),

    #'queue.bind_ok'{} =
        amqp_channel:call(Chan, #'queue.bind' {
                                   queue       = Q,
                                   exchange    = Ex,
                                   routing_key = RK
                                  }).

declare_exchange(Chan, ExDeclare) ->
    #'exchange.declare_ok'{} =
        amqp_channel:call(Chan, ExDeclare).

publish_messages(Chan, Ex, Msgs) ->
    publish_messages(Chan, Ex, <<>>, Msgs).

publish_messages(Chan, Ex, RK, Msgs) ->
    [amqp_channel:call(Chan,
                       #'basic.publish'{exchange = Ex,
                                        routing_key = RK},
                       make_msg(V)) || V <- Msgs].

publish_persisted_messages(Chan, Ex, Msgs) ->
    publish_persisted_messages(Chan, Ex, <<>>, Msgs).

publish_persisted_messages(Chan, Ex, RK, Msgs) ->
    [amqp_channel:call(Chan,
                       #'basic.publish'{exchange = Ex,
                                        routing_key = RK},
                       make_persisted_msg(V)) || V <- Msgs].

consume(Chan, Q, Msgs, Timeout) ->
    #'basic.consume_ok'{} =
        amqp_channel:subscribe(Chan, #'basic.consume'{queue  = Q,
                                                      no_ack = true}, self()),
    collect(length(Msgs), Timeout).


collect(N, Timeout) ->
    collect(0, N, Timeout, []).

collect(N, N, _Timeout, Acc) ->
    {ok, lists:reverse(Acc)};
collect(Curr, N, Timeout, Acc) ->
    receive {#'basic.deliver'{},
             Msg = #amqp_msg{}} ->
            collect(Curr+1, N, Timeout, [Msg | Acc])
    after Timeout ->
            {error, {timeout, Acc}}
    end.

delete_exchange(Ex) ->
    #'exchange.delete' {
       exchange       = Ex
      }.

delete_queue(Q) ->
    #'queue.delete' {
       queue       = Q
      }.

make_queue(Q) ->
    #'queue.declare' {
       queue       = Q
      }.

make_exchange(Ex, Type) ->
    #'exchange.declare'{
       exchange    = Ex,
       type        = Type
      }.

make_msg(V) ->
    #amqp_msg{payload = term_to_binary(V)}.

make_persisted_msg(V) ->
    #amqp_msg{
      props = #'P_basic'{delivery_mode = ?PERSIST_MESSAGE_DELIVERY_MODE},
      payload = term_to_binary(V)
    }.
