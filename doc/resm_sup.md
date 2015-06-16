

# Module resm_sup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)



resm supervisor module.
Copyright (c) 2015, Dmitry Averbakh

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ Dmitry Averbakh ([`adm@ruhub.com`](mailto:adm@ruhub.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td>
Whenever a supervisor is started using supervisor:start_link/[2,3], this function is called by the new process to
find out about restart strategy, maximum restart frequency and child specifications.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the supervisor.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###


<pre><code>
init(Args) -&gt; {ok, {{Strategy, MaxR, MaxT}, [Spec]}}
</code></pre>

<ul class="definitions"><li><code>Args = any()</code></li><li><code>Strategy = <a href="supervisor.md#type-strategy">supervisor:strategy()</a></code></li><li><code>MaxR = non_neg_integer()</code></li><li><code>MaxT = non_neg_integer()</code></li><li><code>Spec = <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a></code></li></ul>


Whenever a supervisor is started using supervisor:start_link/[2,3], this function is called by the new process to
find out about restart strategy, maximum restart frequency and child specifications.
<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, pid()}
</code></pre>
<br />


Starts the supervisor.
