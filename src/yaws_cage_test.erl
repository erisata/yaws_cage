%%% @doc
%%% Test utils for managing Yaws in test cases.
%%%
%%%
%%% == Typical Usage ==
%%%
%%% The needed Yaws servers will typically be started in the
%%% init/end per suite/testcase, for example:
%%% ```
%%% init_per_suite(Config) ->
%%%     yaws_cage_test:ct_init(8080, [
%%%        "/my_app/rest", my_app_rest)
%%%     ], Config).
%%%  end_per_suite(Config) ->
%%%     ok = yaws_cage_test:ct_end(Config).
%%% '''
%%% Then a testcase can call the API at [http://localhost:8080/my_app/rest].
%%%
%%%
%%% == Using it for HTTP Mocks ==
%%%
%%% Typically, the mock server will be initialized as follows (in CT):
%%% ```
%%% init_per_suite(Config) ->
%%%     yaws_cage_test:ct_init(#{8090 => [
%%%         {"/MOCK/some_path", my_app_mock_some_path} % Non existing module.
%%%     ]}, Config).
%%% end_per_suite(Config) ->
%%%     ok = yaws_cage_test:ct_end(Config).
%%% init_per_testcase(_TestCase, Config) ->
%%%     ok = meck:new(my_app_mock_some_path, [non_strict]),
%%%     ok = meck:expect(my_app_mock_some_path, out, fun (Arg) ->
%%%         yaws_cage_rest:out(my_app_mock_some_path, Arg, #{debug => true})
%%%     end),
%%%     Config.
%%% end_per_testcase(_TestCase, _Config) ->
%%%     ok = meck:unload(my_app_mock_some_path).
%%% '''
%%% Then a testcase use it as follows (see the {@link yaws_cage_rest} behaviour):
%%% ```
%%% test_something(Config) ->
%%%     ok = meck:expect(my_app_mock_some_path, handle_request, fun (["resource"], 'GET', _Arg, _Opts) ->
%%%        [{status, 200}, {content, "text/plain", <<"OK!">>}]
%%%     end),
%%%     % 
%%%     % Perform the test during which a call to an external service
%%%     % will be made at [http://localhost:8080/my_app/rest/resource].
%%%     % 
%%%     ok = meck:wait(my_app_mock_some_path, handle_request, [["resource"], 'GET', '_', '_'], 5000),
%%%     1 = meck:num_calls(my_app_mock_some_path, handle_request, [["resource"], 'GET', '_', '_']),
%%%     true = meck:validate(my_app_mock_some_path),
%%%     ok.
%%% '''
%%%
-module(yaws_cage_test).
-export([ct_init/3, ct_init/2, ct_end/1]).

-ignore_xref([
    {?MODULE, ct_init, 2},    % Public API.
    {?MODULE, ct_init, 3},    % Public API.
    {?MODULE, ct_end, 1}      % Public API.
]).


%%  @doc
%%  Start the Yaws server at the specified port with the specified appmods.
%%  Intended to be used in CT `init_per_suite/1', `end_per_suite/1'.
%%
-spec ct_init(
        Port    :: integer(),
        AppMods :: [{string(), module()}],
        Config  :: list()
    ) ->
        NewConfig :: list().

ct_init(Port, AppMods, Config) ->
    ct_init(#{Port => AppMods}, Config).


%%  @doc
%%  Similar to {@link ct_init/2}, except that allows to start yaws
%%  listeners at several ports with sets of different appmods.
%%
-spec ct_init(
        PortAppModMap :: #{integer() => [{string(), module()}]},
        Config        :: list()
    ) ->
        NewConfig :: list().

ct_init(PortAppModMap, Config) ->
    ServerId = erlang:atom_to_list(?MODULE),
    PrivDir = proplists:get_value(priv_dir, Config),
    DocRoot = filename:join(PrivDir, ServerId),
    ok = filelib:ensure_dir(filename:join(DocRoot, "any")),
    ok = case [ ok || {yaws, _, _} <- application:which_applications()] of
        []   -> yaws:start_embedded(DocRoot, [], [], ServerId);
        [ok] -> ok % Already started.
    end,
    GConf = yaws:create_gconf([], ServerId),
    SConf = [
        yaws:create_sconf(DocRoot, [
            {port,       Port},
            {servername, ServerId},
            {listen,     {127, 0, 0, 1}},
            {docroot,    DocRoot},
            {appmods,    AppMods}
        ])
        || {Port, AppMods} <- maps:to_list(PortAppModMap)
    ],
    case yaws_api:setconf(GConf, SConf) of
        ok ->
            ok;
        {error, need_restart} ->
            _ = application:stop(yaws),
            ok = yaws:start_embedded(DocRoot, [], [], ServerId),
            ok = yaws_api:setconf(GConf, SConf)
    end,
    ct:pal("Yaws server started at localhost, appmods: ~p~n", [PortAppModMap]),
    [{yaws_cage_test_server_id, ServerId} | Config].


%%  @doc
%%  Stop all the Yaws listeners, if they were started.
%%  The Yaws application itself is left as running to avoid unnecessary noice in the logs.
%%
-spec ct_end(
        Config  :: list()
    ) ->
        ok.

ct_end(Config) ->
    case proplists:get_value(yaws_cage_test_server_id, Config) of
        undefined ->
            ok;
        ServerId ->
            GConf = yaws:create_gconf([], ServerId),
            ok = yaws_api:setconf(GConf, [])
    end.


