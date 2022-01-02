%% @doc
%% Generic module that does the rule-based validation.
-module(zazanet_validation).

-include("zazanet_logger.hrl").

-export([do/2]).

-type rule() ::
    undefined |
    number |
    nonempty_binary |
    {starts_with, Prefix :: binary()} |
    fun((term()) -> true | string()).
-type lrule() ::
    {'not', pipeline()} | {'or', pipeline(), pipeline()} | {'and', pipeline(), pipeline()}.
-type pipeline() :: [rule() | lrule()].

%% @doc
%% Takes a term to validate along with a {@link pipeline(). validation pipeline}.
%% Throws `error(badarg)' in case of failure.
%% Usage example:
%% ```
%% try
%%     zazanet_validation:validate(Foo, [{starts_with, <<"prefix">>}]),
%%     bar(Foo)
%% catch
%%     error:badarg ->
%%        ...
%% end
%% '''
-spec do(Term :: term(), Pipeline :: pipeline()) -> ok | no_return().
do(_, []) ->
    true;
do(Term, [{'not', Pipeline} | SubPipeline]) ->
    try
        ?MODULE:do(Term, Pipeline),
        error({local, badarg})
    catch
        error:badarg ->
            ?MODULE:do(Term, SubPipeline);
        error:{local, badarg} ->
            error(badarg)
    end;
do(Term, [{'or', LeftPipeline, RightPipeline} | SubPipeline]) ->
    try
        ?MODULE:do(Term, LeftPipeline),
        true
    catch
        error:badarg ->
            ?MODULE:do(Term, RightPipeline),
            ?MODULE:do(SubPipeline)
    end;
do(Term, [{'and', LeftPipeline, RightPipeline} | SubPipeline]) ->
    ?MODULE:do(Term, LeftPipeline),
    ?MODULE:do(Term, RightPipeline),
    ?MODULE:do(Term, SubPipeline);
do(undefined, [undefined | Pipeline]) ->
    ?MODULE:do(undefined, Pipeline);
do(Term, [number | Pipeline]) when is_number(Term) ->
    ?MODULE:do(Term, Pipeline);
do(Term, [nonempty_binary | Pipeline]) when is_binary(Term), byte_size(Term) > 0 ->
    ?MODULE:do(Term, Pipeline);
do(Term, [ValidationRule = {starts_with, Prefix} | Pipeline])
    when is_binary(Term), is_binary(Prefix), byte_size(Term) >= byte_size(Prefix) ->
    case binary:part(Term, {0, byte_size(Prefix)}) of
        Prefix ->
            ?MODULE:do(Term, Pipeline);
        _ ->
            ?LOG_VALIDATION_ERROR(Term, ValidationRule),
            error(badarg)
    end;
do(Term, [F | Pipeline]) when is_function(F, 1) ->
    case F(Term) of
        true ->
            ?MODULE:do(Term, Pipeline);
        Text ->
            ?LOG_VALIDATION_ERROR(Term, Text),
            false
    end;
do(Term, [ValidationRule | _]) ->
    ?LOG_VALIDATION_ERROR(Term, ValidationRule),
    error(badarg).
