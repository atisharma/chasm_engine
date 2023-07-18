To play, just enter some text, saying what you want to do.

For instance,
```
> Splash around at the water's edge.
> Go north
> Go to the crossroads.
> Tell so-and-so about thingy.
```

Lines beginning with **/** are parsed as special commands, otherwise everything is understood as normal narrative text.  
The only exception is lines beginning with `go` which are understood to be an instruction to travel.  
The usual readline shortcuts should be available (up-arrow for previous commands, etc).

- **/help /h**                   Show this helpful text
- **/quit /q /exit**             Quit

- **/take sparkly thing**        Add the item called 'sparkly thing' to your inventory
- **/drop old shoes**            Remove the item called 'old shoes' to your inventory
- **/give lantern to Sonia**     Not implemented yet, but will give the lantern in your inventory to Sonia when it is

- **/hint how do I escape?**     Help if you're stuck
- **/history**                   Print your whole narrative
- **/look**                      Describe the place you're in
- **/map**                       List the exits and their directions
