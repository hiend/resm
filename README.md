

# resm - Resource manager. #

__Authors:__ Averbakh Dmitry ([`adm@ruhub.com`](mailto:adm@ruhub.com)).


### Resm is a simple resource manager with REST API interface and JSON data format

Manager operate with limited resources count, specified in a configuration or environment variables. Resource is a unique rN kind id. Manager provide five operations for resources manipulation: allocate resources for user, deallocate resources, show list of allocated and deallocated resources, show list resources allocated by user and reset allocated resources state.

Resource allocating by username and associated with him up to deallocation. List of allocated resources stored like a tuple list, i.e. [{resource, user}], deallocated resources stored like a simple list. Resource deallocating performed by his unique id. Resources list associated with user you may get by his name.

## Let's get started

### Requirements

[Erlang](http://erlang.org) R16 or higher.

### Installation

Clone the repository:

> git clone git@github.com:hiend/resm.git

Build:

> make

### Run

Production:

> make console

Development:

> make dev-rel console

### Go to [`http://localhost:8008/list`](http://localhost:8008/list) and check that works!
Stop the running application by typing `halt().` into the REPL console.

----------

# REST API
## 1. Allocate
### GET [/allocate/user](http://localhost:8008/allocate/user)
```curl
RETURNS:
    201, r1
    503, Out of resources
```
## 2. Deallocate
### GET [/deallocate/r1](http://localhost:8008/deallocate/r1)
```curl
RETURNS:
    204
    404, Not allocated
```
## 3. List
### GET [/list](http://localhost:8008/list)
```curl
RETURNS:
    200, {"allocated":[],"deallocated":["r1","r2"]}
    200, {"allocated":{"r1":"user"},"deallocated":["r2"]}
```
## 4. User list
### GET [/list/user](http://localhost:8008/list/user)
```curl
RETURNS:
    200, []
    200, ["r1","r2"]
```
## 5. Reset
### GET [/reset](http://localhost:8008/reset)
```curl
RETURNS:
    204
```

## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/hiend/resm/blob/master/doc/resm.md" class="module">resm</a></td></tr>
<tr><td><a href="http://github.com/hiend/resm/blob/master/doc/resm_manager.md" class="module">resm_manager</a></td></tr>
<tr><td><a href="http://github.com/hiend/resm/blob/master/doc/resm_rest.md" class="module">resm_rest</a></td></tr>
<tr><td><a href="http://github.com/hiend/resm/blob/master/doc/resm_sup.md" class="module">resm_sup</a></td></tr></table>

