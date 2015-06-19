%%%---------------------------------------------------------------------------------------------------------------------
%%% @author Dmitry Averbakh <adm@ruhub.com>
%%% @copyright 2015, Dmitry Averbakh
%%% @doc
%%% resm resource manager tests.
%%% @end
%%%---------------------------------------------------------------------------------------------------------------------
-module(resm_tests).
-author("Dmitry Averbakh <adm@ruhub.com>").

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% apps for start
-define(APPS, [compiler, syntax_tools, goldrush, crypto, sasl, lager, ranch, cowlib, cowboy, jiffy, resm]).

%% test states
-define(INITIAL_STATE, {ok,{[],[r1,r2,r3,r4,r5,r6,r7,r8,r9,r10]}}).
-define(ALLOCATED_STATE, {ok,{[{r1,user1},{r2,user2}],[r3,r4,r5,r6,r7,r8,r9,r10]}}).
-define(DEALLOCATED_STATE, {ok,{[{r2,user2}], [r3,r4,r5,r6,r7,r8,r9,r10,r1]}}).

%%%=====================================================================================================================
%%% eunit functions
%%%=====================================================================================================================

%%----------------------------------------------------------------------------------------------------------------------
%% @doc
%% Start test environment.
%% @end
%%----------------------------------------------------------------------------------------------------------------------
setup() ->
    [ok = application:start(App) || App <- ?APPS].

%%----------------------------------------------------------------------------------------------------------------------
%% @doc
%% Stop test environment.
%% @end
%%----------------------------------------------------------------------------------------------------------------------
teardown(_) ->
    [ok = application:stop(App) || App <- lists:reverse(?APPS)].

%%%=====================================================================================================================
%%% test functions
%%%=====================================================================================================================

%%----------------------------------------------------------------------------------------------------------------------
%% @doc
%% Common workflow steps.
%% @end
%%----------------------------------------------------------------------------------------------------------------------
resm_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"Initial list", fun() ->
            ?assertEqual(?INITIAL_STATE, resm_manager:list())
        end},
        {"Allocate", fun() ->
            ?assertEqual({ok,r1}, resm_manager:allocate(user1)),
            ?assertEqual({ok,r2}, resm_manager:allocate(user2))
        end},
        {"Allocated list", fun() ->
            ?assertEqual(?ALLOCATED_STATE, resm_manager:list())
        end},
        {"Allocated list for user1", fun() ->
            ?assertEqual({ok,[r1]}, resm_manager:list(user1))
        end},
        {"Allocated list for anyuser", fun() ->
            ?assertEqual({ok,[]}, resm_manager:list(anyuser))
        end},
        {"Deallocate r1", fun() ->
            ?assertEqual(ok, resm_manager:deallocate(r1))
        end},
        {"Deallocated list", fun() ->
            ?assertEqual(?DEALLOCATED_STATE, resm_manager:list())
        end},
        {"Reset", fun() ->
            ?assertEqual(ok, resm_manager:reset())
        end},
        {"Reseted list", fun() ->
            ?assertEqual(?INITIAL_STATE, resm_manager:list())
        end},
        {"Out of resources error", fun() ->
            [{ok, _} = resm_manager:allocate(user) || _ <- lists:seq(1, 10)],
            ?assertEqual({error, out_of_resources}, resm_manager:allocate(user))
        end},
        {"Not allocated error", fun() ->
            ?assertEqual({error, not_allocated}, resm_manager:deallocate(not_a_resource))
        end},
        {"Not allocated after reset", fun() ->
            resm_manager:reset(),
            ?assertEqual({error, not_allocated}, resm_manager:deallocate(r1)),
            ?assertEqual({ok, []}, resm_manager:list(user))
        end}
     ]}.

%%%=====================================================================================================================
%%% Internal functions
%%%=====================================================================================================================
