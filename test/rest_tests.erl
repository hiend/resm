%%%---------------------------------------------------------------------------------------------------------------------
%%% @author Dmitry Averbakh <adm@ruhub.com>
%%% @copyright 2015, Dmitry Averbakh
%%% @doc
%%% resm rest api handler tests.
%%% @end
%%%---------------------------------------------------------------------------------------------------------------------
-module(rest_tests).
-author("Dmitry Averbakh <adm@ruhub.com>").

%% Includes
-include_lib("eunit/include/eunit.hrl").

%% apps for start
-define(APPS, [compiler, syntax_tools, sasl, lager, ranch, cowlib, cowboy, jiffy, resm, inets]).

%% test states
-define(INITIAL_STATE, "{\"allocated\":{},\"deallocated\":[\"r1\",\"r2\",\"r3\",\"r4\",\"r5\",\"r6\",\"r7\",\"r8\",\"r9\",\"r10\"]}").
-define(ALLOCATED_STATE, "{\"allocated\":{\"r1\":\"user1\",\"r2\":\"user2\"},\"deallocated\":[\"r3\",\"r4\",\"r5\",\"r6\",\"r7\",\"r8\",\"r9\",\"r10\"]}").
-define(DEALLOCATED_STATE, "{\"allocated\":{\"r2\":\"user2\"},\"deallocated\":[\"r3\",\"r4\",\"r5\",\"r6\",\"r7\",\"r8\",\"r9\",\"r10\",\"r1\"]}").

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
%% Common workflow steps through the rest web api.
%% @end
%%----------------------------------------------------------------------------------------------------------------------
rest_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"Initial list", fun() ->
            {ok, {{_HttpVersion, 200, "OK"}, _Headers, ?INITIAL_STATE}} = rest("list")
        end},
        {"Allocate", fun() ->
            {ok, {{_HttpVersion, 201, "Created"}, _Headers, "r1"}} = rest("allocate/user1"),
            {ok, {{_HttpVersion, 201, "Created"}, _Headers, "r2"}} = rest("allocate/user2")
        end},
        {"Allocated list", fun() ->
            {ok, {{_HttpVersion, 200, "OK"}, _Headers, ?ALLOCATED_STATE}} = rest("list")
        end},
        {"Allocated list for user1", fun() ->
            {ok, {{_HttpVersion, 200, "OK"}, _Headers, "[\"r1\"]"}} = rest("list/user1")
        end},
        {"Allocated list for anyuser", fun() ->
            {ok, {{_HttpVersion, 200, "OK"}, _Headers, "[]"}} = rest("list/anyuser")
        end},
        {"Deallocate r1", fun() ->
            {ok, {{_HttpVersion, 204, "No Content"}, _Headers, []}} = rest("deallocate/r1")
        end},
        {"Deallocated list", fun() ->
            {ok, {{_HttpVersion, 200, "OK"}, _Headers, ?DEALLOCATED_STATE}} = rest("list")
        end},
        {"Reset", fun() ->
            {ok, {{_HttpVersion, 204, "No Content"}, _Headers, []}} = rest("reset")
        end},
        {"Reseted list", fun() ->
            {ok, {{_HttpVersion, 200, "OK"}, _Headers, ?INITIAL_STATE}} = rest("list")
        end},
        {"Out of resources error", fun() ->
            [{ok, _} = rest("allocate/user") || _ <- lists:seq(1, 10)],
            {ok, {{_HttpVersion, 503, "Service Unavailable"}, _Headers, "Out of resources"}} = rest("allocate/user")
        end},
        {"Not allocated error", fun() ->
            {ok, {{_HttpVersion, 404, "Not Found"}, _Headers, "Not allocated"}} = rest("deallocate/not_a_resource")
        end},
        {"Not allocated after reset", fun() ->
            {ok, _} = rest("reset"),
            {ok, {{_HttpVersion, 200, "OK"}, _Headers, "[]"}} = rest("list/user")
        end},
        {"Not allocated after reset", fun() ->
            {ok, _} = rest("reset"),
            {ok, {{_HttpVersion, 404, "Not Found"}, _Headers, "Not allocated"}} = rest("deallocate/r1")
        end},
        {"Any request error", fun() ->
            {ok, {{_HttpVersion, 400, "Bad Request"}, _Headers, "Bad request"}} = rest("make/war")
        end}
    ]}.

%%%=====================================================================================================================
%%% Internal functions
%%%=====================================================================================================================

%%----------------------------------------------------------------------------------------------------------------------
%% @doc
%% HTTP request to rest handler.
%% @end
%%----------------------------------------------------------------------------------------------------------------------
rest(Op) when is_list(Op) ->
    httpc:request("http://127.0.0.1:8008/" ++ Op).
