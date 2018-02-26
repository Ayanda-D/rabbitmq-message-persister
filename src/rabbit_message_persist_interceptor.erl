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
%% The Initial Developer of the Original Code is Erlang Solutions Ltd.
%% Copyright (c) 2007-2018 Erlang Solutions Ltd.  All rights reserved.
%%

-module(rabbit_message_persist_interceptor).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").
-include_lib("rabbit_message_persister.hrl").

-import(rabbit_basic, [header/2]).

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
    undefined.

description() ->
    [{description,
      <<"Ensures delivery mode is set to '2' on messages as they enter RabbitMQ">>}].

intercept(#'basic.publish'{} = Method, Content, _IState) ->
    DecodedContent = rabbit_binary_parser:ensure_content_decoded(Content),
    Content2 = ensure_delivery_mode_2(DecodedContent),
    {Method, Content2};

intercept(Method, Content, _VHost) ->
    {Method, Content}.

applies_to() ->
    ['basic.publish'].

%%----------------------------------------------------------------------------
ensure_delivery_mode_2(#content{properties = #'P_basic'{delivery_mode = 2}}
  = Content) ->
    Content;

ensure_delivery_mode_2(#content{properties = Props} = Content) ->
    %% we need to reset properties_bin = none so the new properties
    %% get serialized when deliverying the message.
    Content#content{properties =
      Props#'P_basic'{delivery_mode = ?PERSIST_MESSAGE_DELIVERY_MODE},
                      properties_bin = none}.
