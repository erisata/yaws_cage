

# Module yaws_cage_test #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Test utils for managing Yaws in test cases.

<a name="description"></a>

## Description ##


### <a name="Typical_Usage">Typical Usage</a> ###

The needed Yaws servers will typically be started in the
init/end per suite/testcase, for example:

```
   init_per_suite(Config) ->
       yaws_cage_test:ct_init(8080, [
          "/my_app/rest", my_app_rest)
       ], Config).
    end_per_suite(Config) ->
       ok = yaws_cage_test:ct_end(Config).
```

Then a testcase can call the API at [`http://localhost:8080/my_app/rest`](http://localhost:8080/my_app/rest).


### <a name="Using_it_for_HTTP_Mocks">Using it for HTTP Mocks</a> ###

Typically, the mock server will be initialized as follows (in CT):

```
   init_per_suite(Config) ->
       yaws_cage_test:ct_init(#{8090 => [
           {"/MOCK/some_path", my_app_mock_some_path} % Non existing module.
       ]}, Config).
   end_per_suite(Config) ->
       ok = yaws_cage_test:ct_end(Config).
   init_per_testcase(_TestCase, Config) ->
       ok = meck:new(my_app_mock_some_path, [non_strict]),
       ok = meck:expect(my_app_mock_some_path, out, fun (Arg) ->
           yaws_cage_rest:out(my_app_mock_some_path, Arg, #{debug => true})
       end),
       Config.
   end_per_testcase(_TestCase, _Config) ->
       ok = meck:unload(my_app_mock_some_path).
```

Then a testcase use it as follows (see the [`yaws_cage_rest`](yaws_cage_rest.md) behaviour):

```
   test_something(Config) ->
       ok = meck:expect(my_app_mock_some_path, handle_request, fun (["resource"], 'GET', _Arg, _Opts) ->
          [{status, 200}, {content, "text/plain", <<"OK!">>}]
       end),
       %
       % Perform the test during which a call to an external service
       % will be made at [http://localhost:8080/my_app/rest/resource].
       %
       ok = meck:wait(my_app_mock_some_path, handle_request, [["resource"], 'GET', '_', '_'], 5000),
       1 = meck:num_calls(my_app_mock_some_path, handle_request, [["resource"], 'GET', '_', '_']),
       true = meck:validate(my_app_mock_some_path),
       ok.
```

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ct_end-1">ct_end/1</a></td><td>
Stop all the Yaws listeners, if they were started.</td></tr><tr><td valign="top"><a href="#ct_init-2">ct_init/2</a></td><td>
Similar to <a href="#ct_init-2"><code>ct_init/2</code></a>, except that allows to start yaws
listeners at several ports with sets of different appmods.</td></tr><tr><td valign="top"><a href="#ct_init-3">ct_init/3</a></td><td>
Start the Yaws server at the specified port with the specified appmods.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="ct_end-1"></a>

### ct_end/1 ###

<pre><code>
ct_end(Config::list()) -&gt; ok
</code></pre>
<br />

Stop all the Yaws listeners, if they were started.
The Yaws application itself is left as running to avoid unnecessary noice in the logs.

<a name="ct_init-2"></a>

### ct_init/2 ###

<pre><code>
ct_init(PortAppModMap::#{integer() =&gt; [{string(), module()}]}, Config::list()) -&gt; NewConfig::list()
</code></pre>
<br />

Similar to [`ct_init/2`](#ct_init-2), except that allows to start yaws
listeners at several ports with sets of different appmods.

<a name="ct_init-3"></a>

### ct_init/3 ###

<pre><code>
ct_init(Port::integer(), AppMods::[{string(), module()}], Config::list()) -&gt; NewConfig::list()
</code></pre>
<br />

Start the Yaws server at the specified port with the specified appmods.
Intended to be used in CT `init_per_suite/1`, `end_per_suite/1`.

