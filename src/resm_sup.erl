%%%---------------------------------------------------------------------------------------------------------------------
%%% @author Dmitry Averbakh <adm@ruhub.com>
%%% @copyright 2015, Dmitry Averbakh
%%% @doc
%%% resm supervisor module.
%%% @end
%%%---------------------------------------------------------------------------------------------------------------------
-module(resm_sup).
-author("Dmitry Averbakh <adm@ruhub.com>").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=====================================================================================================================
%%% API functions
%%%=====================================================================================================================

%%----------------------------------------------------------------------------------------------------------------------
%% @doc
%% Starts the supervisor.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec start_link() -> {'ok', pid()}.

start_link() ->
    {ok, _Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%=====================================================================================================================
%%% Supervisor callbacks
%%%=====================================================================================================================

%%----------------------------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3], this function is called by the new process to
%% find out about restart strategy, maximum restart frequency and child specifications.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec init(Args) -> {ok, {{Strategy, MaxR, MaxT}, [Spec]}} when
    Args     :: any(),
    Strategy :: supervisor:strategy(),
    MaxR     :: non_neg_integer(),
    MaxT     :: non_neg_integer(),
    Spec     :: supervisor:child_spec().

init(_Args) ->
    RestartStrategy = {one_for_one, 5, 10},
    ChildSpec = {resm_manager, {resm_manager, start_link, []}, permanent, 2000, worker, [resm_manager]},

    {ok, {RestartStrategy, [ChildSpec]}}.

%%%=====================================================================================================================
%%% Internal functions
%%%=====================================================================================================================
