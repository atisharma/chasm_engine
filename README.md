# Chasm - CHAracter State Manager

Chasm is a ***generative text adventure game*** in a ***world you
specify***. It uses generative artificial intelligence to generate
scenes and characters as you play. Unlike simply role-playing in
ChatGPT or similar, important state persists (locations, characters,
dialogue etc.)

You can use it with a local model (LLaMA derivative like
Wizard-Vicuna) or OpenAI's models. See the config file for examples.
It works very well with a 30B LLaMA model and acceptably with a 13B
one. It 'kind-of-works' with a 7B LLaMA model, but the results aren't
great.

Chasm is still being written. It's already pretty great though,
with a good model.


## Features

* [x] specify initial world with a short description
* [x] persistent world / locations
* [x] fuzzy matching names of locations
* [x] continue / save file for game
* [x] persistent items
* [x] character inventory
* [x] take, drop items
* [ ] modify/use items
* [ ] persistent global event memory (vector db)
* [ ] per-character dialogue memory (vector db)
* [ ] per-character event memory
* [ ] per-character quests
* [x] protagonist is just another player but with a "human" model
* [ ] command-line parameters to override config
* [ ] world editor for manual world construction


## Installing and running

### World information

Place a text file named after your world in a subdirectory called
"worlds" (see the config file).

The world information (text file) should contain two or three
sentences about the world that won't change (not specific places,
characters or items), such as general location or the period in
history. They are universal and invariant context for your journey.

For example, set `world = "worlds/New York"` in your config file
and create a text file `worlds/New York.txt` with the contents
```
The setting is 1930's New York, and surrounding areas. There are many buildings specific to the area.
[genre: realist urban fiction]
```
This will create a world called 'New York' and scenes appropriate to it.


### Character cards

If you want to override your or any other character's attributes permanently,
create a file `worlds/New York/characters/Hero.json` (for a character name of
Hero) with the contents
```
{
    "name": "Hero",
    "appearance": "Heroic.",
    "backstory": "Comes from a long line of heroes.",
    "voice": "Heroic.",
    "traits": "Heroism.",
    "dislikes": "Not being heroic.",
    "motivation": "To be heroic."
}
```
reflecting the desired values. Leave out fields and they'll be automatically generated. The `name` field is ignored (since it's implicit in the filename).


## Interface

- [x] management done by config files
- [x] terminal interface
- [ ] web chat interface, since should be remotely available?
- [ ] map display?


## Problems / bugs

There are still many.


### Inspiration

- Zork-like games. Though I never played Zork.
- https://github.com/QuangBK/generativeAgent_LLM
- https://github.com/atisharma/llama_farm
