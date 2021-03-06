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

-define(PERSIST_MESSAGE_DELIVERY_MODE,    2).
-define(NONPERSIST_MESSAGE_DELIVERY_MODE, 1).
-define(DEFAULT_PERSISTANCE_MODE,         ?PERSIST_MESSAGE_DELIVERY_MODE).
-define(APP,                              rabbitmq_message_persister).
