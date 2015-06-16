%%%---------------------------------------------------------------------------------------------------------------------
%%% @author Dmitry Averbakh <adm@ruhub.com>
%%% @copyright 2015, Dmitry Averbakh
%%% @doc
%%% resm resource manager module.
%%% @end
%%%---------------------------------------------------------------------------------------------------------------------
-module(resm_manager).
-author("Dmitry Averbakh <adm@ruhub.com>").

-behavior(gen_server).

%% Includes
-include("resm.hrl").

%% OTP API
-export([start_link/0]).

%% Public API
-export([allocate/1, deallocate/1, list/0, list/1, reset/0]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

%%----------------------------------------------------------------------------------------------------------------------

-type resource()  :: atom().

-record(resources, {
    allocated = [] :: [{atom(), resource()}],
    deallocated    :: [resource()]}).
-type resources_rec() :: #resources{}.

%%%=====================================================================================================================
%%% OTP API functions
%%%=====================================================================================================================

%%----------------------------------------------------------------------------------------------------------------------
%% @doc
%% Starts the server.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec start_link() -> {'ok', pid()}.

start_link() ->
    {ok, _Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%=====================================================================================================================
%%% Public API functions
%%%=====================================================================================================================

%%----------------------------------------------------------------------------------------------------------------------
%% @doc
%% Allocate resource for user.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec allocate(User :: atom()) -> {'ok', resource()} | {'error', out_of_resources}.

allocate(User) when is_atom(User) ->
    gen_server:call(?MODULE, {allocate, User}).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc
%% Deallocate resource.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec deallocate(Resource :: resource()) -> 'ok' | {'error', not_allocated}.

deallocate(Resource) when is_atom(Resource) ->
    gen_server:call(?MODULE, {deallocate, Resource}).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc
%% Get allocated and deallocated resources lists.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec list() -> {'ok', {[{atom(), resource()}], [resource()]}}.

list() ->
    gen_server:call(?MODULE, list).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc
%% Get resources list allocated by user.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec list(User :: atom()) -> {'ok', [{resource(), atom()}]}.

list(User) when is_atom(User) ->
    gen_server:call(?MODULE, {list, User}).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc
%% Deallocate all resources.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec reset() -> ok.

reset() ->
    gen_server:cast(?MODULE, reset).

%%%=====================================================================================================================
%%% gen_server callbacks
%%%=====================================================================================================================

%%----------------------------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec init(Args  :: any()) -> {ok, State :: resources_rec()}.

init(_Args) ->
    {ok, #resources{deallocated = init_resources()}}.

%%----------------------------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling sync call messages.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec handle_call(Request, From, State) -> {reply, term(), State} when
    Request :: term(),
    From    :: any(),
    State   :: resources_rec().

handle_call({allocate, _User}, _From, #resources{deallocated = []} = State) ->
    {reply, {error, out_of_resources}, State};

handle_call({allocate, User}, _From, #resources{deallocated = [Resource|Deallocated]} = State) ->
    {reply, {ok, Resource}, #resources{
        allocated = State#resources.allocated ++ [{Resource, User}],
        deallocated = Deallocated}};

handle_call({deallocate, _Resource}, _From, #resources{allocated = []} = State) ->
    {reply, {error, not_allocated}, State};

handle_call({deallocate, Resource}, _From, #resources{} = State) ->
    case lists:keymember(Resource, 1, State#resources.allocated) of
        true ->
            {reply, ok, #resources{
                allocated = lists:keydelete(Resource, 1, State#resources.allocated),
                deallocated = State#resources.deallocated ++ [Resource]}};
        false ->
            {reply, {error, not_allocated}, State}
    end;

handle_call(list, _From, #resources{} = State) ->
    {reply, {ok, {State#resources.allocated, State#resources.deallocated}}, State};

handle_call({list, _User}, _From, #resources{allocated = []} = State) ->
    {reply, {ok, []}, State};

handle_call({list, User}, _From, #resources{} = State) ->
    {reply, {ok, [Resource || {Resource, Owner} <- State#resources.allocated, Owner =:= User]}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling async cast messages.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec handle_cast(Request, State) -> {noreply, State} when
    Request :: term(),
    State   :: resources_rec().

handle_cast(reset, _State) ->
    {noreply, #resources{deallocated = init_resources()}};

handle_cast(_Request, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec handle_info(Info, State) -> {noreply, State} when
    Info  :: any(),
    State :: resources_rec().

handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec code_change(OldVsn, State, Extra) -> {ok, State} when
    OldVsn :: any(),
    State  :: resources_rec(),
    Extra  :: any().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to terminate. It should be the opposite of Module:init/1
%% and do any necessary cleaning up. When it returns, the gen_server terminates with Reason. The return value is
%% ignored.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec terminate(Reason :: any(), State :: any()) -> 'ok'.

terminate(_Reason, _State) ->
    ok.

%%%=====================================================================================================================
%%% Internal functions
%%%=====================================================================================================================

%%----------------------------------------------------------------------------------------------------------------------
%% @doc
%% Convert integer to 'rN' kind atom.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec to_rN(N :: integer()) -> atom().

to_rN(N) when is_integer(N) ->
    list_to_atom(lists:concat(["r", N])).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc
%% Init resources list.
%% @end
%%----------------------------------------------------------------------------------------------------------------------

-spec init_resources() -> [atom()].

init_resources() ->
    [to_rN(X) || X <- lists:seq(1, resm:get_env("RESOURCES_COUNT", resources_count, ?RESM_RESOURCES_COUNT))].
