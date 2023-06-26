# Chasm - CHAracter State Manager

Chasm is a *generative text adventure game*. It uses generative
artificial intelligence to generate scenes and characters as you
play. Unlike role-playing in ChatGPT or similar, important state
persists (locations, characters, dialogue etc.)

You can use it with a local model (LLaMA derivative like
Wizard-Vicuna) or OpenAI's models. See the config file for examples.

Chasm is still being written.

## Features

[x] specify initial world with a short description
[ ] continue / save file for dialogue
[x] persistent world / locations
[x] fuzzy matching names of locations
[ ] persistent items
[ ] persistent global event memory (vector db)
[ ] per character event memory (vector db)
[ ] per character dialogue memory (vector db)
[ ] character inventory (current state only; toml?)
  * [x] [ ] protagonist is just another player but with a "human" model
[ ] thus it could generalise to MMP worlds
    * how should many people access at the same time?


## State

### World information

The world information (text file) should contain two or three
sentences about the world (not specific places, characters or items)
that won't change, such as general location or the decade.  They are
universal and invariant context for your journey.

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


## Installing and running

Place a text file named after your world in a subdirectory called "worlds" (see the config file).

## Problems

- consistent naming of generated location names


### Inspiration

- https://github.com/QuangBK/generativeAgent_LLM
- https://github.com/atisharma/llama_farm
