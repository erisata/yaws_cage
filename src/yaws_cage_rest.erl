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
%%%         yaws_cage_rest:out(?MODULE, Arg).
%%%
%%%     handle_request(["check"], 'GET', Arg) ->
%%%         {content, 200, <<"ok!">>};
%%%     handle_request(Path, Method, Arg) ->
%%%         yaws_cage_rest:handle_unsupported(?MODULE, Path, Method, Arg).
%%%
%%%
%%% See `http://www.infoq.com/articles/vinoski-erlang-rest`.
%%%
-module(yaws_cage_rest).
-compile([{parse_transform, lager_transform}]).
-export([out/2, handle_unsupported/4]).
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
        Arg     :: #arg{}
    ) -> term().



%%% ============================================================================
%%% API Functions
%%% ============================================================================

%%
%%  Entry point for Yaws appmod.
%%
out(Module, Arg = #arg{req = Req, client_ip_port = {Ip, _Port}, opaque = Opaque}) ->
    Path = path_tokens(Arg),
    Method = yaws_api:http_request_method(Req),
    case lists:member({"yaws_cage_rest_appmod_debug", "true"}, Opaque) of
        true ->
            lager:debug(
                "Appmod ~p=~p: ip=~p, method=~p, path=~p",
                [Module, appmod_path(Arg), encode_ip(Ip), Method, Path]
            );
        false ->
            ok
    end,
    Module:handle_request(Path, Method, Arg).


%%
%%  Default fallback for unsupported cases.
%%
handle_unsupported(Module, Path, Method, Arg) ->
    respond_unknown(Module, Path, Method, Arg).



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
%%  Prints IP.
%%
encode_ip({One, Two, Three, Four}) ->
    lists:flatten(io_lib:format("~B.~B.~B.~B", [One, Two, Three, Four])).


%%
%%  Generic method for responding to unknown paths.
%%
respond_unknown(Module, Path, 'GET' = Method, _Args) ->
    respond_error(404, "Unknown resource", Module, Path, Method, '#');

respond_unknown(Module, Path, Method, _Args) ->
    respond_error(400, "Bad request", Module, Path, Method, '#').


%%
%%  Respond to request with error message.
%%
respond_error(Status, Message, Module, Path, Method, Args) ->
    lager:warning(string:concat(Message, " in ~p, path=~p, method=~p, args=~p"), [Module, Path, Method, Args]),
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
