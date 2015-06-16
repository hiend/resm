

# Module resm_app #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)



resm application module.
Copyright (c) 2015, Dmitry Averbakh

__Behaviours:__ [`application`](application.md).

__Authors:__ Dmitry Averbakh ([`adm@ruhub.com`](mailto:adm@ruhub.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-2">start/2</a></td><td>
This function is called whenever an application is started using application:start/[1,2], and should start the
processes of the application.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>
This function is called whenever an application has stopped.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start-2"></a>

### start/2 ###


<pre><code>
start(StartType::any(), StartArgs::any()) -&gt; {ok, pid()}
</code></pre>
<br />


This function is called whenever an application is started using application:start/[1,2], and should start the
processes of the application. If the application is structured according to the OTP design principles as a
supervision tree, this means starting the top supervisor of the tree.
<a name="stop-1"></a>

### stop/1 ###


<pre><code>
stop(State::any()) -&gt; ok
</code></pre>
<br />


This function is called whenever an application has stopped. It is intended to be the opposite of Module:start/2 and
should do any necessary cleaning up. The return value is ignored.
