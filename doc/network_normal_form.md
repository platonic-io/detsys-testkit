#### Network normal form

One of the difficult parts of in the simulation pseudo code above is the ability
to pause and resume processes/components when they get stuck needing network
traffic. Here's an example process:

```
q = [...]
    send M
    [...]
    msg := recv
    if msg == X
    then [...]
    else [...]
```

Here we first need run `q` and do stuff (possibly sending messages), get stuck
at the `recv`, somehow capture the continuation of the then and else branches
and resume it once `q` is sent a message.

To make this simpler one could express processes on what I liked to call
"network normal form". The main idea is to model a process as a function from
received message to a set of reply messages, i.e.:

```
  p : ReceivedMessage -> Set ReplyMessage
```

The idea of resuming then is very easy, when `p` gets a message, simply apply
the function `p` to the message and then schedule all the replies to other
processes, rinse and repeat.

What if you want the process to send something before receving its first
message? Or how can you handle "background" stuff like periodically pinging the
leader?

This is where the second part of the normal form idea comes in, namely the
background `tick` thread. Each process has one `tick` thread associated with it
and on this thread theres a simple tick handler which is a function from the
current time to a set of messages:

```
  tickHandler : Timestamp -> Set ReplyMessages
```

The tick thread gets spawned before the main loop which receieves messages of a
process, i.e. something like:

```
main =
  fork
    (while true {
      time := getTime;
      msgs := tickHandler time;
      send msgs;
      sleep })

  while true {
    msg := recv
    replies := p msg
    send replies
  }
```

It should be fairly clear that all networking programs can be rewritten on
normal form, hence the name.
