

# Module resm #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)



resm common tools module.
Copyright (c) 2015, Dmitry Averbakh

__Authors:__ Dmitry Averbakh ([`adm@ruhub.com`](mailto:adm@ruhub.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_env-3">get_env/3</a></td><td>
Get environment (system or module) variable.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_env-3"></a>

### get_env/3 ###


<pre><code>
get_env(VarName, Key, Default) -&gt; term()
</code></pre>

<ul class="definitions"><li><code>VarName = string()</code></li><li><code>Key = atom()</code></li><li><code>Default = term()</code></li></ul>


Get environment (system or module) variable.
