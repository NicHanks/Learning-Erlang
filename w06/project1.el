-module(kv_store).
-export([start/0, set/2, get/1]).

start() ->
    spawn(fun() -> init([]) end).

init(Data) ->
    receive
        {set, Key, Value} ->
            NewData = update_or_insert(Data, {Key, Value}),
            init(NewData);
        {get, Key, Pid} ->
            case lists:keyfind(Key, 1, Data) of
                {Key, Value} ->
                    Pid ! {self(), {ok, Value}};
                false ->
                    Pid ! {self(), not_found}
            end,
            init(Data);
        stop ->
            ok
    end.

set(Pid, Key, Value) ->
    Pid ! {set, Key, Value}.

get(Pid, Key) ->
    Pid ! {get, Key, self()},
    receive
        {Pid, Response} -> Response
    end.

update_or_insert(Data, {Key, Value}) ->
    case lists:keyfind(Key, 1, Data) of
        false ->
            [{Key, Value} | Data];
        {Key, _} = Found ->
            lists:keyreplace(Key, 1, Data, {Key, Value})
    end.


1> c(kv_store).
{ok,kv_store}
2> Pid = kv_store:start().
<0.84.0>
3> kv_store:set(Pid, hello, "world").
{set,hello,"world"}
4> kv_store:get(Pid, hello).
{ok,"world"}
5> kv_store:get(Pid, not_found).
not_found
