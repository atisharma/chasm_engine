# CHASM - CHAracter State Manager

A rough draft of a generative text adventure game that uses a large language model.


## Intended Features

- specify things with toml or json files
  * initial world info, setting etc
  * locations
  * characters
- persistent global event memory (vector db)
- per character event memory (vector db)
- per character dialogue memory (vector db)
- character inventory (current state only; toml?)
- protagonist is just another player but with a "human" model
- thus it could generalise to MMP worlds
  * how should many people access at the same time?


## State

### Saved state
```
world-name/
    state.json             -- general world description, style etc. Read only.
    locations/
        location-name-1/
            state.json     -- lstate
            events.db      -- historical events for location 1
        location-name-2/
            ...
    characters/
        character-name-1/
            state.json     -- cstate
            events.db      -- historical events
            chat.db        -- conversation snippets
        character-name-2/
            ...
```


### Live state (in-memory)

See `types.hy`

- `chat`, per-character dialogue



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

Functions which determine if a reference refers to an existing object or a new one.

```
is-same-space: location, location -> score
is-same-character: character, character -> score
nearby: location -> [location]
```

We can consider geographical location (`nearby`) when determining available locations from the current location.


### Generation

Functions which generate a new NPC or location

```
new-location: coords -> location
new-character: coords -> character
```

`new-location` should graft in available locations at each direction. Like an 8-way linked list.


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
