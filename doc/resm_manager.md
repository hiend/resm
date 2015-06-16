

# Module resm_manager #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



resm resource manager module.
Copyright (c) 2015, Dmitry Averbakh

__Authors:__ Dmitry Averbakh ([`adm@ruhub.com`](mailto:adm@ruhub.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-resource">resource()</a> ###



<pre><code>
resource() = atom()
</code></pre>





### <a name="type-resources_rec">resources_rec()</a> ###



<pre><code>
resources_rec() = #resources{}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#allocate-1">allocate/1</a></td><td>
Allocate resource for user.</td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td>
Convert process state when code is changed.</td></tr><tr><td valign="top"><a href="#deallocate-1">deallocate/1</a></td><td>
Deallocate resource.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td>
Handling sync call messages.</td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td>
Handling async cast messages.</td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td>
Handling all non call/cast messages.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>
Initializes the server.</td></tr><tr><td valign="top"><a href="#init_resources-0">init_resources/0*</a></td><td>
Init resources list.</td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td>
Get allocated and deallocated resources lists.</td></tr><tr><td valign="top"><a href="#list-1">list/1</a></td><td>
Get resources list allocated by user.</td></tr><tr><td valign="top"><a href="#reset-0">reset/0</a></td><td>
Deallocate all resources.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td>
This function is called by a gen_server when it is about to terminate.</td></tr><tr><td valign="top"><a href="#to_rN-1">to_rN/1*</a></td><td>
Convert integer to 'rN' kind atom.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="allocate-1"></a>

### allocate/1 ###


<pre><code>
allocate(User::atom()) -&gt; {ok, <a href="#type-resource">resource()</a>} | {error, out_of_resources}
</code></pre>
<br />


Allocate resource for user.
<a name="code_change-3"></a>

### code_change/3 ###


<pre><code>
code_change(OldVsn, State, Extra) -&gt; {ok, State}
</code></pre>

<ul class="definitions"><li><code>OldVsn = any()</code></li><li><code>State = <a href="#type-resources_rec">resources_rec()</a></code></li><li><code>Extra = any()</code></li></ul>


Convert process state when code is changed.
<a name="deallocate-1"></a>

### deallocate/1 ###


<pre><code>
deallocate(Resource::<a href="#type-resource">resource()</a>) -&gt; ok | {error, not_allocated}
</code></pre>
<br />


Deallocate resource.
<a name="handle_call-3"></a>

### handle_call/3 ###


<pre><code>
handle_call(Request, From, State) -&gt; {reply, term(), State}
</code></pre>

<ul class="definitions"><li><code>Request = term()</code></li><li><code>From = any()</code></li><li><code>State = <a href="#type-resources_rec">resources_rec()</a></code></li></ul>


Handling sync call messages.
<a name="handle_cast-2"></a>

### handle_cast/2 ###


<pre><code>
handle_cast(Request, State) -&gt; {noreply, State}
</code></pre>

<ul class="definitions"><li><code>Request = term()</code></li><li><code>State = <a href="#type-resources_rec">resources_rec()</a></code></li></ul>


Handling async cast messages.
<a name="handle_info-2"></a>

### handle_info/2 ###


<pre><code>
handle_info(Info, State) -&gt; {noreply, State}
</code></pre>

<ul class="definitions"><li><code>Info = any()</code></li><li><code>State = <a href="#type-resources_rec">resources_rec()</a></code></li></ul>


Handling all non call/cast messages.
<a name="init-1"></a>

### init/1 ###


<pre><code>
init(Args::any()) -&gt; {ok, State::<a href="#type-resources_rec">resources_rec()</a>}
</code></pre>
<br />


Initializes the server.
<a name="init_resources-0"></a>

### init_resources/0 * ###


<pre><code>
init_resources() -&gt; [atom()]
</code></pre>
<br />


Init resources list.
<a name="list-0"></a>

### list/0 ###


<pre><code>
list() -&gt; {ok, {[{atom(), <a href="#type-resource">resource()</a>}], [<a href="#type-resource">resource()</a>]}}
</code></pre>
<br />


Get allocated and deallocated resources lists.
<a name="list-1"></a>

### list/1 ###


<pre><code>
list(User::atom()) -&gt; {ok, [{<a href="#type-resource">resource()</a>, atom()}]}
</code></pre>
<br />


Get resources list allocated by user.
<a name="reset-0"></a>

### reset/0 ###


<pre><code>
reset() -&gt; ok
</code></pre>
<br />


Deallocate all resources.
<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, pid()}
</code></pre>
<br />


Starts the server.
<a name="terminate-2"></a>

### terminate/2 ###


<pre><code>
terminate(Reason::any(), State::any()) -&gt; ok
</code></pre>
<br />


This function is called by a gen_server when it is about to terminate. It should be the opposite of Module:init/1
and do any necessary cleaning up. When it returns, the gen_server terminates with Reason. The return value is
ignored.
<a name="to_rN-1"></a>

### to_rN/1 * ###


<pre><code>
to_rN(N::integer()) -&gt; atom()
</code></pre>
<br />


Convert integer to 'rN' kind atom.
