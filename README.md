# CHASM - CHAracter State Manager

Chasm is a *generative text adventure game*. It uses generative artificial intelligence to generate scenes and characters as you play. Unlike role-playing in ChatGPT or similar, important state persists (locations, characters, dialogue etc.)

It is still being written.

## Features

- specify initial world with a short description
- persistent world / locations
- persistent global event memory (vector db) (TBD)
- per character event memory (vector db) (TBD)
- per character dialogue memory (vector db) (TBD)
- character inventory (current state only; toml?) (TBD)
- protagonist is just another player but with a "human" model (TBD)
- thus it could generalise to MMP worlds (TBD)
  * how should many people access at the same time?


## State

### Persistent state

- locations, characters, items. See `types.hy` and `state.hy`


### Generated

- scene descriptions
- map connectivity
- puzzles
- chat, per-character dialogue


### Actions

Functions which modify state.

```
move: modifies a character's coords
use: character, item -> event
interact-location: cstate, lstate -> event
interact-character: [cstate] -> [cstate]
say: dialogue, cstate -> dialogue, cstate
```


### Matching

We need functions which determine if a reference refers to an existing object or a new one.


### Generation

Functions which generate a new NPC or location

```
new-location: coords -> location
new-character: coords -> character
```

`location.new` grafts in available locations at each direction. Like an 8-way linked list.


## Interface

- Web chat interface, since should be remotely available?
- Management done by config file
- Map display


## Problems

- consistent naming of generated location names
- fuzzy matching names of characters/locations (jaro-winkler)


### Inspiration

https://github.com/QuangBK/generativeAgent_LLM
https://github.com/atisharma/llama_farm
