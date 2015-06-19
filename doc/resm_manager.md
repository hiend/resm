

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


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#allocate-1">allocate/1</a></td><td>
Allocate resource for user.</td></tr><tr><td valign="top"><a href="#deallocate-1">deallocate/1</a></td><td>
Deallocate resource.</td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td>
Get allocated and deallocated resources lists.</td></tr><tr><td valign="top"><a href="#list-1">list/1</a></td><td>
Get resources list allocated by user.</td></tr><tr><td valign="top"><a href="#reset-0">reset/0</a></td><td>
Deallocate all resources.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="allocate-1"></a>

### allocate/1 ###


<pre><code>
allocate(User::atom()) -&gt; {ok, <a href="#type-resource">resource()</a>} | {error, out_of_resources}
</code></pre>
<br />


Allocate resource for user.
<a name="deallocate-1"></a>

### deallocate/1 ###


<pre><code>
deallocate(Resource::<a href="#type-resource">resource()</a>) -&gt; ok | {error, not_allocated}
</code></pre>
<br />


Deallocate resource.
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
