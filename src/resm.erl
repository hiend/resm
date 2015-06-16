%%%---------------------------------------------------------------------------------------------------------------------
%%% @author Dmitry Averbakh <adm@ruhub.com>
%%% @copyright 2015, Dmitry Averbakh
%%% @doc
%%% resm common tools module.
%%% @end
%%%---------------------------------------------------------------------------------------------------------------------
-module(resm).
-author("Dmitry Averbakh <adm@ruhub.com>").

-export([get_env/3]).

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
