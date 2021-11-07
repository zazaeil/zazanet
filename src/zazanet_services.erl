-module(zazanet_services).

-behaviour(gen_server).

-include("zazanet_services.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-export([get/0, put/1]).

-record(state, {services = #{}}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(get, _From, State=#state{services=Services}) ->
    {reply, [Service || {Service, _} <- maps:values(Services)], State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({put, ZazanetService=#zazanet_service{id=ID, ttl=TTL}}, State=#state{services=Services}) ->
    case maps:get(ID, Services, undefined) of
        undefined -> ok;
        {ok, {ZazanetService, OldTimer}} ->
            case timer:cancel(OldTimer) of
                {ok, cancel} -> ok;
                {error, Error} ->
                    logger:notice(#{event => put, error => Error})
            end
    end,
    {ok, Timer} = timer:send_after(TTL, {expired, ID}),
    {noreply, State#state{services = maps:put(ID, {ZazanetService, Timer}, Services)}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({expired, ID}, State=#state{services=Services}) ->
    {noreply, State#state{services=maps:remove(ID, Services)}};
handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, #state{services=Services}) ->
    [timer:cancel(Timer) || {_, Timer} <- maps:values(Services)],
    ok.

get() ->
    gen_server:call(?MODULE, get).

put(ZazanetService) ->
    gen_server:cast(?MODULE, {put, ZazanetService}).
