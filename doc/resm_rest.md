

# Module resm_rest #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)



resm http rest api handler module.
Copyright (c) 2015, Dmitry Averbakh

__Authors:__ Dmitry Averbakh ([`adm@ruhub.com`](mailto:adm@ruhub.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#handle-2">handle/2</a></td><td>
This function for handling the request.</td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td>
This function is meant for initialization.</td></tr><tr><td valign="top"><a href="#terminate-3">terminate/3</a></td><td>
This function for cleaning up.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="handle-2"></a>

### handle/2 ###


<pre><code>
handle(Req, State) -&gt; {ok, Req, State}
</code></pre>

<ul class="definitions"><li><code>Req = <a href="cowboy_req.md#type-req">cowboy_req:req()</a></code></li><li><code>State = atom()</code></li></ul>


This function for handling the request. It receives the request and the state.
<a name="init-3"></a>

### init/3 ###


<pre><code>
init(X1::{tcp, http}, Req, State) -&gt; {ok, Req, State}
</code></pre>

<ul class="definitions"><li><code>Req = <a href="cowboy_req.md#type-req">cowboy_req:req()</a></code></li><li><code>State = any()</code></li></ul>


This function is meant for initialization. It receives information about the transport and protocol used, along
with the handler options from the dispatch list, and allows you to upgrade the protocol if needed.
<a name="terminate-3"></a>

### terminate/3 ###


<pre><code>
terminate(Reason::any(), Req::any(), State::any()) -&gt; ok
</code></pre>
<br />


This function for cleaning up. It also receives the request and the state.
