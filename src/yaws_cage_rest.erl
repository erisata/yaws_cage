%/--------------------------------------------------------------------
%| Copyright 2016 Erisata, UAB (Ltd.)
%|
%| Licensed under the Apache License, Version 2.0 (the "License");
%| you may not use this file except in compliance with the License.
%| You may obtain a copy of the License at
%|
%|     http://www.apache.org/licenses/LICENSE-2.0
%|
%| Unless required by applicable law or agreed to in writing, software
%| distributed under the License is distributed on an "AS IS" BASIS,
%| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%| See the License for the specific language governing permissions and
%| limitations under the License.
%\--------------------------------------------------------------------

%%%
%%% YAWS Appmod for basic REST services.
%%%
%%% You can use this module as follows:
%%%
%%%     -module(my_rest).
%%%     -behaviour(yaws_cage_rest).
%%%     -export([out/1, handle_request/4]).
%%%     -include_lib("yaws/include/yaws_api.hrl").
%%%
%%%     out(Arg) ->
%%%         yaws_cage_rest:out(?MODULE, Arg, _Opts = []).
%%%
%%%     handle_request(["check"], 'GET', _Arg, _Opts) ->
%%%         {content, 200, <<"ok!">>};
%%%     handle_request(Path, Method, Arg, _Opts) ->
%%%         yaws_cage_rest:handle_unsupported(?MODULE, Path, Method, Arg).
%%%
%%%
%%% See `http://www.infoq.com/articles/vinoski-erlang-rest`.
%%%
-module(yaws_cage_rest).
-compile([{parse_transform, lager_transform}]).
-export([out/3, handle_unsupported/4, appmod_path/1, serve_file/4, serve_priv_file/4]).
-include_lib("yaws/include/yaws_api.hrl").


%%% ============================================================================
%%% Callback definitions.
%%% ============================================================================

%%
%%  Handle all the requests.
%%
-callback handle_request(
        Path    :: [string()],
        Method  :: atom(),
        Arg     :: #arg{},
        Opts    :: map()
    ) -> term().



%%% ============================================================================
%%% API Functions
%%% ============================================================================

%%
%%  This function should be called from the yaws appmod, the Module:out/1 function.
%%  It will delegate the calls to the Module:handle_request/4 function.
%%
-spec out(
        Module :: module(),
        Arg    :: #arg{},
        Opts   :: #{debug => boolean()}
    ) ->
        YawsResponse :: term().

out(Module, Arg = #arg{req = #http_request{method = Method}, client_ip_port = {Ip, Port}}, Opts) ->
    Path = path_tokens(Arg),
    case Opts of
        #{debug := true} ->
            lager:debug(
                "Appmod ~p=~p: ip=~p:~p, method=~p, path=~p",
                [Module, appmod_path(Arg), encode_ip(Ip), Port, Method, Path]
            );
        _ ->
            ok
    end,
    Module:handle_request(Path, Method, Arg, Opts).


%%
%%  Default fallback for unsupported cases.
%%
handle_unsupported(Module, Path, Arg, Opts) ->
    respond_unknown(Module, Path, Arg, Opts).



%%
%%  Returns a path, at which the appmod is mounted.
%%
%%  <AppmodPath>/<AppmodData> = <PrePath><Appmod>/AppmodData = <ServerPath>
%%
appmod_path(#arg{server_path = ServerPath, appmoddata = undefined}) ->
    ServerPath;

appmod_path(#arg{server_path = ServerPath, appmoddata = "/"}) ->
    string:substr(ServerPath, 1, length(ServerPath) - 1);

appmod_path(#arg{server_path = ServerPath, appmoddata = AppmodData}) ->
    string:substr(ServerPath, 1, length(ServerPath) - length(AppmodData) - 1).


%%
%%
%%
serve_priv_file(Module, Path, Arg, Opts) ->
    serve_file(Module, [priv_dir(Module) | Path], Arg, Opts).


%%
%%
%%
serve_file(Module, Path, Arg, Opts) ->
    FileName = filename:join(Path),
    case file:read_file(FileName) of
        {ok, Content} ->
            ContentType = case Opts of
                #{content_type := ProvidedContentType} -> ProvidedContentType;
                _                                      -> yaws_api:mime_type(FileName)
            end,
            ResponseContent = case Opts of
                #{variables := Variables} when map_size(Variables) > 0 ->
                    ReplaceVariable = fun (VarName, VarValue, Binary) ->
                        binary:replace(Binary, VarName, VarValue, [global])
                    end,
                    BinContent = erlang:iolist_to_binary(Content),
                    maps:fold(ReplaceVariable, BinContent, Variables);
                _ ->
                    Content
            end,
            [
                {status, 200},
                {content, ContentType, ResponseContent}
            ];
        {error, enoent} ->
            respond_unknown(Module, Path, Arg, Opts);
        {error, eacces} ->
            respond_error(401, "Insuficient acces rights", Module, Path, Arg);
        {error, Reason} ->
            respond_error(400, lists:concat(["Error: ", Reason]), Module, Path, Arg)
    end.


%%% ============================================================================
%%% Internal functions
%%% ============================================================================

%%
%%  Returns path tokens, that are under the appmod path.
%%
path_tokens(#arg{appmoddata = undefined}) ->
    [];

path_tokens(#arg{appmoddata = AppmodUri}) ->
    path_tokens_normalize(string:tokens(AppmodUri, "/")).


%%
%%  Checks path for the path_traversal attack.
%%
path_tokens_normalize(Tokens) ->
    path_tokens_normalize([], Tokens).

path_tokens_normalize(Normalized, []) ->
    lists:reverse(Normalized);

path_tokens_normalize([], [".." | _Tokens]) ->
    erlang:exit(path_traversal);

path_tokens_normalize([_ | Normalized], [".." | Tokens]) ->
    path_tokens_normalize(Normalized, Tokens);

path_tokens_normalize(Normalized, [Token | Tokens]) when Token =/= ".." ->
    path_tokens_normalize([Token | Normalized], Tokens).


%%
%%  Prints IP.
%%
encode_ip({One, Two, Three, Four}) ->
    lists:flatten(io_lib:format("~B.~B.~B.~B", [One, Two, Three, Four])).


%%
%%  Generic method for responding to unknown paths.
%%
respond_unknown(Module, Path, 'GET' = Method, Arg = #arg{req = #http_request{method = Method}}) ->
    case Method of
        'GET' -> respond_error(404, "Unknown resource", Module, Path, Arg);
        _     -> respond_error(400, "Bad request",      Module, Path, Arg)
    end.


%%
%%  Respond to request with error message.
%%
respond_error(Status, Message, Module, Path, #arg{req = #http_request{method = Method}}) ->
    lager:warning(string:concat(Message, " in ~p, path=~p, method=~p"), [Module, Path, Method]),
    [
        {status, Status},
        {ehtml, error_html(Status, Message)}
    ].


%%
%%  Format error message.
%%
error_html(_Status, Message) ->
    [
        {h1, [], "ERROR!"},
        {p, [], Message}
    ].


%%
%%  Returns a path to the file in the application's priv directory.
%%  Application is determined from the module.
%%
priv_dir(Module) ->
    case application:get_application(Module) of
        {ok, AppName} ->
            case code:priv_dir(AppName) of
                {error, bad_name} -> "priv"; % To allow testing without creating whole app.
                Dir               -> Dir
            end;
        undefined ->
            "priv" % To allow testing without creating whole app.
    end.



%%% ============================================================================
%%% Test cases for internal functions.
%%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

path_tokens_normalize_test_() ->
    [
        ?_assertEqual([              ], path_tokens_normalize([])),
        ?_assertEqual(["some", "path"], path_tokens_normalize(["some", "path"])),
        ?_assertEqual(["some"        ], path_tokens_normalize(["some", "path", ".."])),
        ?_assertEqual([              ], path_tokens_normalize(["some", "path", "..", ".."])),
        ?_assertEqual(["path"        ], path_tokens_normalize(["some", "..", "path"])),
        ?_assertExit(path_traversal,    path_tokens_normalize(["some", "path", "..", "..", ".."]))
    ].


-endif.