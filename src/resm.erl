%%%---------------------------------------------------------------------------------------------------------------------
%%% @author Dmitry Averbakh <adm@ruhub.com>
%%% @copyright 2015, Dmitry Averbakh
%%% @doc
%%% resm application module.
%%% @end
%%%---------------------------------------------------------------------------------------------------------------------
-module(resm).
-author("Dmitry Averbakh <adm@ruhub.com>").

-behaviour(application).

%% Includes
-include("resm.hrl").

%% Application callbacks
-export([start/2, stop/1, get_env/3]).

%%%=====================================================================================================================
%%% Application callbacks
%%%=====================================================================================================================

%%----------------------------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using application:start/[1,2], and should start the
%% processes of the application. If the application is structured according to the OTP design principles as a
%% supervision tree, this means starting the top supervisor of the tree.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec start(StartType :: any(), StartArgs :: any()) -> {'ok', pid()}.

start(_StartType, _StartArgs) ->
    % Routes
    Allocate = {"/allocate/:user", resm_rest, allocate},
    Deallocate = {"/deallocate/:resource", resm_rest, deallocate},
    List = {"/list", resm_rest, list},
    ListUser = {"/list/:user", resm_rest, list_user},
    Reset = {"/reset", resm_rest, reset},
    Any = {'_', resm_rest, any},

    % Cowboy params
    Host = {'_', [Allocate, Deallocate, List, ListUser, Reset, Any]},
    Port = {port, resm:get_env("PORT", port, ?RESM_PORT)},
    Dispatcher = {dispatch, cowboy_router:compile([Host])},
    Env = {env, [Dispatcher]},

    % Start rest API webserver
    {ok, _CPid} = cowboy:start_http(?MODULE, 10, [Port], [Env]),

    % Start supervisor
    {ok, _SPid} = resm_sup:start_link().

%%----------------------------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec stop(State :: any()) -> 'ok'.

stop(_State) ->
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% @doc
%% Get environment (system or module) variable.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec get_env(VarName, Key, Default) -> term() when
    VarName :: string(),
    Key     :: atom(),
    Default :: term().

get_env(VarName, Key, Default) when is_list(VarName), is_atom(Key) ->
    case os:getenv(VarName) of
        false ->
            application:get_env(?MODULE, Key, Default);
        Value ->
            Value
    end.

%%%=====================================================================================================================
%%% Internal functions
%%%=====================================================================================================================
