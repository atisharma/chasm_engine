# Chasm - CHAracter State Manager

Chasm is a *generative text adventure game*. It uses generative
artificial intelligence to generate scenes and characters as you
play. Unlike role-playing in ChatGPT or similar, important state
persists (locations, characters, dialogue etc.)

You can use it with a local model (LLaMA derivative like
Wizard-Vicuna) or OpenAI's models. See the config file for examples.

Chasm is still being written. It's already pretty great though.


## Features

* [x] specify initial world with a short description
* [x] continue / save file for dialogue
* [x] persistent world / locations
* [x] fuzzy matching names of locations
* [x] persistent items
* [ ] persistent global event memory (vector db)
* [ ] per character event memory (vector db)
* [ ] per character dialogue memory (vector db)
* [.] character inventory (current state only; toml?)
* [x] protagonist is just another player but with a "human" model


## State

### World information

The world information (text file) should contain two or three
sentences about the world that won't change (not specific places,
characters or items), such as general location or the period in
history. They are universal and invariant context for your journey.

### Persistent state

- locations
- characters
- items
- events


### Generated

- scene descriptions
- map connectivity
- puzzles
- chat, per-character dialogue


### Actions


### Generation


## Interface

- Web chat interface, since should be remotely available?
- Management done by config files
- Map display?


## Installing and running

Place a text file named after your world in a subdirectory called "worlds" (see the config file).
Optionally, place character cards in a characters subdirectory within the created game folder.
E.g. `worlds/mygame/characters/Alice.json`, which would contain something like this
```json
{
    "name": "Alice",
    "appearance": "...",
    "backstory": "...",
    "voice": "...",
    "traits": "...",
    "motivation": "...",
    "dislikes": "..."
}
```
with the desired values. Leave out fields and they'll be automatically generated.

## Problems

There are lots still.


### Inspiration

- https://github.com/QuangBK/generativeAgent_LLM
- https://github.com/atisharma/llama_farm
