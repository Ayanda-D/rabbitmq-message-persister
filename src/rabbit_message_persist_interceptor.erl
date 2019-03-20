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
%% The Developer of this component is Erlang Solutions, Ltd.
%% Copyright (c) 2007-2018 Erlang Solutions Ltd.  All rights reserved.
%%

-module(rabbit_message_persist_interceptor).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").
-include_lib("rabbit_message_persister.hrl").

-behaviour(rabbit_channel_interceptor).

-export([description/0, intercept/3, applies_to/0, init/1]).

-rabbit_boot_step({?MODULE,
                   [{description, "message persisting interceptor"},
                    {mfa, {rabbit_registry, register,
                           [channel_interceptor,
                            <<"message persisting interceptor">>, ?MODULE]}},
                    {cleanup, {rabbit_registry, unregister,
                               [channel_interceptor,
                                <<"message persisting interceptor">>]}},
                    {requires, rabbit_registry},
                    {enables, recovery}]}).

init(_Ch) ->
    put(?APP, rabbit_misc:get_env(?APP, delivery_mode, ?DEFAULT_PERSISTANCE_MODE)),
    undefined.

description() ->
    [{description,
      <<"Ensures delivery mode is set to '2' on messages as they enter RabbitMQ">>}].

intercept(#'basic.publish'{} = Method, Content, _IState) ->
    DecodedContent = rabbit_binary_parser:ensure_content_decoded(Content),
    Content2 = set_delivery_mode(DecodedContent, get(?APP)),
    {Method, Content2};

intercept(Method, Content, _VHost) ->
    {Method, Content}.

applies_to() ->
    ['basic.publish'].

%%----------------------------------------------------------------------------
set_delivery_mode(#content{properties =
  #'P_basic'{delivery_mode = ?PERSIST_MESSAGE_DELIVERY_MODE}} = Content,
    ?PERSIST_MESSAGE_DELIVERY_MODE) ->
    Content;

set_delivery_mode(#content{properties =
  #'P_basic'{delivery_mode = ?NONPERSIST_MESSAGE_DELIVERY_MODE}} = Content,
    ?NONPERSIST_MESSAGE_DELIVERY_MODE) ->
    Content;

set_delivery_mode(#content{properties = Props} = Content, DeliveryMode)
  when is_integer(DeliveryMode) ->
    %% we need to reset properties_bin = none so the new properties
    %% get serialized when deliverying the message.
    Content#content{properties =
      Props#'P_basic'{delivery_mode = DeliveryMode},
                      properties_bin = none}.
