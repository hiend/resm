%%%---------------------------------------------------------------------------------------------------------------------
%%% @author Dmitry Averbakh <adm@ruhub.com>
%%% @copyright 2015, Dmitry Averbakh
%%% @doc
%%% resm http rest api handler module.
%%% @end
%%%---------------------------------------------------------------------------------------------------------------------
-module(resm_rest).
-author("Dmitry Averbakh <adm@ruhub.com>").

-behavior(cowboy_http_handler).

%% Includes
-include("resm.hrl").

%% cowboy callbacks
-export([init/3, handle/2, terminate/3]).

%% response headers
-define(CONTENT_ENCODING, {<<"content-encoding">>, <<"utf-8">>}).
-define(CONTENT_TYPE, {<<"content-type">>, <<"application/json">>}).

%%%=====================================================================================================================
%%% cowboy callbacks
%%%=====================================================================================================================

%%----------------------------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% This function is meant for initialization. It receives information about the transport and protocol used, along
%% with the handler options from the dispatch list, and allows you to upgrade the protocol if needed.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec init({tcp, http}, Req, State) -> {'ok', Req, State} when
    Req   :: cowboy_req:req(),
    State :: any().

init({tcp, http}, Req, State) ->
    {ok, Req, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% This function for handling the request. It receives the request and the state.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec handle(Req, State) -> {'ok', Req, State} when
    Req   :: cowboy_req:req(),
    State :: atom().

handle(Req, allocate) ->
    {User, Req1} = cowboy_req:binding(user, Req),
    {ok, Replied} = case resm_manager:allocate(binary_to_atom(User, utf8)) of
        {ok, Resource} ->
            cowboy_req:reply(201, [?CONTENT_ENCODING, ?CONTENT_TYPE], atom_to_binary(Resource, utf8), Req1);
        {error, out_of_resources} ->
            cowboy_req:reply(503, [], <<"Out of resources">>, Req1)
    end,
    {ok, Replied, allocate};

handle(Req, deallocate) ->
    {Resource, Req1} = cowboy_req:binding(resource, Req),
    {ok, Replied} = case resm_manager:deallocate(binary_to_atom(Resource, utf8)) of
        ok ->
            {ok, Req1};
        {error, not_allocated} ->
            cowboy_req:reply(404, [], <<"Not allocated">>, Req1)
    end,
    {ok, Replied, deallocate};

handle(Req, list) ->
    {ok, {Allocated, Deallocated}} = resm_manager:list(),
    EJSON = {[{allocated, {Allocated}}, {deallocated, Deallocated}]},
    {ok, Replied} = cowboy_req:reply(200, [?CONTENT_ENCODING, ?CONTENT_TYPE], jiffy:encode(EJSON), Req),
    {ok, Replied, list};

handle(Req, list_user) ->
    {User, Req1} = cowboy_req:binding(user, Req),
    {ok, Resources} = resm_manager:list(binary_to_atom(User, utf8)),
    {ok, Replied} = cowboy_req:reply(200, [?CONTENT_ENCODING, ?CONTENT_TYPE], jiffy:encode(Resources), Req1),
    {ok, Replied, list_user};

handle(Req, reset) ->
    ok = resm_manager:reset(),
    {ok, Req, reset};

handle(Req, any) ->
    {ok, Replied} = cowboy_req:reply(400, [], <<"Bad request">>, Req),
    {ok, Replied, any}.

%%----------------------------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% This function for cleaning up. It also receives the request and the state.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec terminate(Reason :: any(), Req :: any(), State :: any()) -> 'ok'.

terminate(_Reason, _Req, _State) ->
    ok.

%%%=====================================================================================================================
%%% Internal functions
%%%=====================================================================================================================
